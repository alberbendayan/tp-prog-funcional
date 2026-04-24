{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.Runtime
  ( BotM(..)
  , runBotM
  , Env(..)
  , BotState(..)
  , BotError(..)
  , initialBotState
  , checkConnectivityOrThrow
  , fetchMarketSnapshotOrThrow
  , executeRound
  ) where

import Bot.Config (Config)
import Bot.Domain
import Exchange.Interface (Exchange(..), ExchangeError(..))
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.State.Strict (StateT, MonadState, runStateT, modify)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)
import qualified Data.Map.Strict as M

data Env e = Env
  { envConfig    :: Config
  , envExchange  :: e
  , envStateRef  :: IORef BotState
  , envStartTime :: UTCTime
  }

data BotState = BotState
  { bsRounds               :: [RoundResult]
  , bsLastRoundResult      :: Maybe RoundResult
  , bsBalances             :: Map Asset Double
  , bsOpenOrders           :: [OrderStep]
  , bsRoundCount           :: Int
  , bsPnlAccumulated       :: Map Asset Double
  , bsErrorsPerRound       :: [Int]
  , bsLastFetchedBalances  :: Map Asset Double
  , bsTradeHistory         :: [PersistedRound]
  }

initialBotState :: BotState
initialBotState = BotState
  { bsRounds              = []
  , bsLastRoundResult     = Nothing
  , bsBalances            = M.empty
  , bsOpenOrders          = []
  , bsRoundCount          = 0
  , bsPnlAccumulated      = M.empty
  , bsErrorsPerRound      = []
  , bsLastFetchedBalances = M.empty
  , bsTradeHistory        = []
  }

data BotError
  = BotExchangeError String
  | BotExecutionError String
  | BotConfigError String
  deriving (Show, Eq)

newtype BotM e a = BotM
  { unBotM :: ReaderT (Env e) (StateT BotState (ExceptT BotError IO)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader (Env e), MonadState BotState, MonadError BotError, MonadIO)

runBotM :: Env e -> BotState -> BotM e a -> IO (Either BotError (a, BotState))
runBotM env st (BotM m) =
  runExceptT $ runStateT (runReaderT m env) st

extractStepInputAmount :: OrderStep -> AssetQty
extractStepInputAmount (OrderStep pair _ (QtyBase  qty)) = AssetQty (base  pair) qty
extractStepInputAmount (OrderStep pair _ (QtyQuote qty)) = AssetQty (quote pair) qty

buildRoundStatusFromErrors :: [String] -> RoundStatus
buildRoundStatusFromErrors []   = RoundSuccess
buildRoundStatusFromErrors errs = RoundPartial errs

calculateFinalOutputAmount :: AssetQty -> [Fill] -> AssetQty
calculateFinalOutputAmount amtIn [] = amtIn
calculateFinalOutputAmount _     fills =
    let lastFill = last fills
    in AssetQty (quote (fillPair lastFill))
                (fillAmountBase lastFill * unPrice (fillPrice lastFill))

buildRoundResult :: MarketSnapshot -> AssetQty -> [Fill] -> [String] -> Either String RoundResult
buildRoundResult snapshot amtIn fills errs =
    let amtOut = calculateFinalOutputAmount amtIn fills
        deltas = roundBalanceDeltasFromParts amtIn amtOut fills
    in do
      validateSameAsset amtIn amtOut
      netPnl <- netPnlUsdtFromDeltas snapshot deltas
      Right RoundResult
        { roundFills      = fills
        , roundAmountIn   = amtIn
        , roundAmountOut  = amtOut
        , roundNetPnlUsdt = netPnl
        , roundStatus     = buildRoundStatusFromErrors errs
        }

executeStepsSequentially
  :: (MonadIO m, MonadError BotError m, Exchange e)
  => e -> [OrderStep] -> [Fill] -> m ([Fill], [String])
executeStepsSequentially _  []     acc = return (reverse acc, [])
executeStepsSequentially ex (s:ss) acc = executeOrder ex s >>= onResult
  where
    onResult (Left  err)  = handleOrderError err
    onResult (Right fill) = executeStepsSequentially ex ss (fill : acc)

    handleOrderError (ExchangeOrderError msg) =
      return (reverse acc, [msg])
    handleOrderError (ExchangeConnError msg) =
      failCritical "Fallo critico de conectividad durante ejecucion de orden" msg
    handleOrderError (ExchangeFetchError msg) =
      failCritical "Fallo critico de API durante ejecucion de orden" msg

    failCritical label msg = throwError (BotExchangeError (label ++ ": " ++ msg))

checkConnectivityOrThrow :: Exchange e => BotM e ()
checkConnectivityOrThrow = do
    env <- ask
    conn <- checkConnectivity (envExchange env)
    case conn of
      Left err ->
        throwError $ BotExchangeError ("Error de conectividad: " ++ show err)
      Right False ->
        throwError $ BotExchangeError "Error de conectividad: checkConnectivity devolvio False"
      Right True ->
        return ()

fetchMarketSnapshotOrThrow :: Exchange e => [Asset] -> BotM e MarketSnapshot
fetchMarketSnapshotOrThrow assets = do
    env <- ask
    snapshot <- fetchMarketSnapshot (envExchange env) assets
    case snapshot of
      Left err -> throwError $ BotExchangeError ("Error obteniendo mercado: " ++ show err)
      Right s  -> return s

executeRound :: Exchange e => MarketSnapshot -> ExecutionPlan -> BotM e RoundResult
executeRound snapshot plan = do
    env <- ask
    let steps = executionPlanSteps plan
        amtIn = extractStepInputAmount (head steps)
    modify $ \s -> s { bsOpenOrders = steps }
    (fills, errs) <- executeStepsSequentially (envExchange env) steps []
    result <- case buildRoundResult snapshot amtIn fills errs of
      Left err -> throwError (BotExecutionError ("No se pudo calcular PnL neto USDT: " ++ err))
      Right rr -> return rr
    modify $ updateStateWithRound result
    return result

updateStateWithRound :: RoundResult -> BotState -> BotState
updateStateWithRound rr st =
    st
      { bsRounds = bsRounds st ++ [rr]
      , bsLastRoundResult = Just rr
      , bsBalances = mergeAssetMaps (bsBalances st) (roundBalanceDeltas rr)
      , bsOpenOrders = []
      , bsRoundCount = bsRoundCount st + 1
      , bsPnlAccumulated = mergeAssetMaps (bsPnlAccumulated st) (roundNetPnlMap rr)
      , bsErrorsPerRound = bsErrorsPerRound st ++ [roundErrorCount rr]
      }

mergeAssetMaps :: Map Asset Double -> Map Asset Double -> Map Asset Double
mergeAssetMaps = M.unionWith (+)

roundNetPnlMap :: RoundResult -> Map Asset Double
roundNetPnlMap rr = M.singleton USDT (roundNetPnlUsdt rr)

roundErrorCount :: RoundResult -> Int
roundErrorCount rr =
    case roundStatus rr of
      RoundSuccess     -> 0
      RoundFailed _    -> 1
      RoundPartial err -> length err

roundBalanceDeltas :: RoundResult -> Map Asset Double
roundBalanceDeltas rr =
    roundBalanceDeltasFromParts (roundAmountIn rr) (roundAmountOut rr) (roundFills rr)

roundBalanceDeltasFromParts :: AssetQty -> AssetQty -> [Fill] -> Map Asset Double
roundBalanceDeltasFromParts amtIn amtOut fills =
    let baseDeltas =
          [ (qtyAsset amtIn, - qtyAmount amtIn)
          , (qtyAsset amtOut, qtyAmount amtOut)
          ]
        feeDeltas = map (\f -> (fillFeeAsset f, - fillFee f)) fills
    in M.fromListWith (+) (baseDeltas ++ feeDeltas)

netPnlUsdtFromDeltas :: MarketSnapshot -> Map Asset Double -> Either String Double
netPnlUsdtFromDeltas snapshot deltas =
    sum <$> traverse valueEntry (M.toList deltas)
  where
    valueEntry :: (Asset, Double) -> Either String Double
    valueEntry (asset, deltaQty) = do
      rate <- assetUsdtRate snapshot asset
      Right (deltaQty * rate)

assetUsdtRate :: MarketSnapshot -> Asset -> Either String Double
assetUsdtRate _ USDT = Right 1
assetUsdtRate snapshot asset =
    directRate `orElse` inverseRate
  where
    quotes = snapshotQuotes snapshot
    directPair = Pair asset USDT
    inversePair = Pair USDT asset

    directRate = case M.lookup directPair quotes of
      Just q  -> Right (midPrice q)
      Nothing -> Left $ "Sin cotización directa a USDT para " ++ show asset

    inverseRate = case M.lookup inversePair quotes of
      Just q ->
        let mid = midPrice q
        in if mid <= 0
           then Left $ "Cotización inválida para " ++ show inversePair
           else Right (1 / mid)
      Nothing -> Left $ "Sin cotización inversa a USDT para " ++ show asset

    orElse :: Either String a -> Either String a -> Either String a
    orElse (Right x) _ = Right x
    orElse (Left _) y  = y

midPrice :: PairQuote -> Double
midPrice q = (unPrice (bidPrice q) + unPrice (askPrice q)) / 2

validateSameAsset :: AssetQty -> AssetQty -> Either String ()
validateSameAsset amtIn amtOut
  | qtyAsset amtIn == qtyAsset amtOut = Right ()
  | otherwise =
      Left $
        "roundAmountIn/out tienen assets distintos: in="
          ++ show (qtyAsset amtIn)
          ++ ", out="
          ++ show (qtyAsset amtOut)
