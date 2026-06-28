{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , netPnlUsdtFromDeltas
  ) where

import Bot.Config (Config)
import Bot.Domain
import Bot.Pricing (assetUsdtRateWhenBuying, assetUsdtRateWhenSelling)
import Exchange.Interface (Exchange(..), ExchangeError(..))
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.State.Strict (StateT, MonadState, runStateT, modify)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Env e = Env
  { envConfig    :: Config
  , envExchange  :: e
  , envStateRef  :: IORef BotState
  , envStartTime :: UTCTime
  }

data BotState = BotState
  { bsLastRoundResult      :: Maybe RoundResult
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
  { bsLastRoundResult     = Nothing
  , bsBalances            = M.empty
  , bsOpenOrders          = []
  , bsRoundCount          = 0
  , bsPnlAccumulated      = M.empty
  , bsErrorsPerRound      = []
  , bsLastFetchedBalances = M.empty
  , bsTradeHistory        = []
  }

data BotError
  = BotExchangeError T.Text
  | BotExecutionError T.Text
  | BotConfigError T.Text
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

buildRoundStatusFromErrors :: [T.Text] -> RoundStatus
buildRoundStatusFromErrors []   = RoundSuccess
buildRoundStatusFromErrors errs = RoundPartial errs

calculateFinalOutputAmount :: AssetQty -> [Fill] -> AssetQty
calculateFinalOutputAmount amtIn [] = amtIn
calculateFinalOutputAmount _     fills =
    let lastFill = last fills
    in AssetQty (quote (fillPair lastFill))
                (fillAmountBase lastFill * unPrice (fillPrice lastFill))

buildRoundResult :: MarketSnapshot -> AssetQty -> [Fill] -> [T.Text] -> Either T.Text RoundResult
buildRoundResult snapshot amtIn fills errs =
    let amtOut = calculateFinalOutputAmount amtIn fills
        deltas = roundBalanceDeltasFromParts amtIn amtOut fills
    in do
      validateSameAsset amtIn amtOut
      netPnl <- netPnlUsdtFromDeltas snapshot deltas
      let startAsset = qtyAsset amtIn
          netStart = M.findWithDefault 0 startAsset deltas
      Right RoundResult
        { roundFills       = fills
        , roundAmountIn    = amtIn
        , roundAmountOut   = amtOut
        , roundNetPnlStart = netStart
        , roundNetPnlUsdt  = netPnl
        , roundStatus      = buildRoundStatusFromErrors errs
        }

executeStepsSequentially
  :: (MonadIO m, MonadError BotError m, Exchange e)
  => e -> [OrderStep] -> [Fill] -> m ([Fill], [T.Text])
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

    failCritical label msg = throwError (BotExchangeError (label <> ": " <> msg))

checkConnectivityOrThrow :: Exchange e => BotM e ()
checkConnectivityOrThrow = do
    env <- ask
    conn <- checkConnectivity (envExchange env)
    case conn of
      Left err ->
        throwError $ BotExchangeError ("Error de conectividad: " <> T.pack (show err))
      Right False ->
        throwError $ BotExchangeError "Error de conectividad: checkConnectivity devolvio False"
      Right True ->
        return ()

fetchMarketSnapshotOrThrow :: Exchange e => [Asset] -> BotM e MarketSnapshot
fetchMarketSnapshotOrThrow assets = do
    env <- ask
    snapshot <- fetchMarketSnapshot (envExchange env) assets
    case snapshot of
      Left err -> throwError $ BotExchangeError ("Error obteniendo mercado: " <> T.pack (show err))
      Right s  -> return s

executeRound :: Exchange e => MarketSnapshot -> ExecutionPlan -> BotM e RoundResult
executeRound snapshot plan = do
    env <- ask
    let steps = executionPlanSteps plan
        amtIn = extractStepInputAmount (head steps)
    modify $ \s -> s { bsOpenOrders = steps }
    (fills, errs) <- executeStepsSequentially (envExchange env) steps []
    result <- case buildRoundResult snapshot amtIn fills errs of
      Left err -> throwError (BotExecutionError ("No se pudo calcular PnL de la ronda: " <> err))
      Right rr -> return rr
    modify $ updateStateWithRound result
    return result

updateStateWithRound :: RoundResult -> BotState -> BotState
updateStateWithRound rr st =
    st
      { bsLastRoundResult = Just rr
      , bsBalances = mergeAssetMaps (bsBalances st) (roundBalanceDeltas rr)
      , bsOpenOrders = []
      , bsRoundCount = bsRoundCount st + 1
      , bsPnlAccumulated = mergeAssetMaps (bsPnlAccumulated st) (roundNetPnlMap rr)
      , bsErrorsPerRound = bsErrorsPerRound st ++ [roundErrorCount rr]
      }

mergeAssetMaps :: Map Asset Double -> Map Asset Double -> Map Asset Double
mergeAssetMaps = M.unionWith (+)

roundNetPnlMap :: RoundResult -> Map Asset Double
roundNetPnlMap rr =
  M.singleton (qtyAsset (roundAmountIn rr)) (roundNetPnlStart rr)

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

netPnlUsdtFromDeltas :: MarketSnapshot -> Map Asset Double -> Either T.Text Double
netPnlUsdtFromDeltas snapshot deltas =
    sum <$> traverse valueEntry (M.toList deltas)
  where
    valueEntry :: (Asset, Double) -> Either T.Text Double
    valueEntry (asset, deltaQty)
      | abs deltaQty <= 1e-18 = Right 0
      | deltaQty > 0 = do
          rate <- assetUsdtRateWhenSelling snapshot asset
          Right (deltaQty * unUsdtRate rate)
      | otherwise = do
          rate <- assetUsdtRateWhenBuying snapshot asset
          Right (deltaQty * unUsdtRate rate)

validateSameAsset :: AssetQty -> AssetQty -> Either T.Text ()
validateSameAsset amtIn amtOut
  | qtyAsset amtIn == qtyAsset amtOut = Right ()
  | otherwise =
      Left $
        "roundAmountIn/out tienen assets distintos: in="
          <> T.pack (show (qtyAsset amtIn))
          <> ", out="
          <> T.pack (show (qtyAsset amtOut))
