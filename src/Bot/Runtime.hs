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

buildRoundResult :: AssetQty -> [Fill] -> [String] -> RoundResult
buildRoundResult amtIn fills errs = RoundResult
    { roundFills     = fills
    , roundAmountIn  = amtIn
    , roundAmountOut = calculateFinalOutputAmount amtIn fills
    , roundStatus    = buildRoundStatusFromErrors errs
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

executeRound :: Exchange e => ExecutionPlan -> BotM e RoundResult
executeRound plan = do
    env <- ask
    let steps = executionPlanSteps plan
        amtIn = extractStepInputAmount (head steps)
    modify $ \s -> s { bsOpenOrders = steps }
    (fills, errs) <- executeStepsSequentially (envExchange env) steps []
    let result = buildRoundResult amtIn fills errs
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
      , bsPnlAccumulated = mergeAssetMaps (bsPnlAccumulated st) (roundPnlMap rr)
      , bsErrorsPerRound = bsErrorsPerRound st ++ [roundErrorCount rr]
      }

mergeAssetMaps :: Map Asset Double -> Map Asset Double -> Map Asset Double
mergeAssetMaps = M.unionWith (+)

roundPnlMap :: RoundResult -> Map Asset Double
roundPnlMap rr =
    case roundPnl rr of
      Left _ -> M.empty
      Right pnl -> M.singleton (qtyAsset pnl) (qtyAmount pnl)

roundErrorCount :: RoundResult -> Int
roundErrorCount rr =
    case roundStatus rr of
      RoundSuccess     -> 0
      RoundFailed _    -> 1
      RoundPartial err -> length err

roundBalanceDeltas :: RoundResult -> Map Asset Double
roundBalanceDeltas rr =
    let amtIn = roundAmountIn rr
        amtOut = roundAmountOut rr
        baseDeltas =
          [ (qtyAsset amtIn, - qtyAmount amtIn)
          , (qtyAsset amtOut, qtyAmount amtOut)
          ]
        feeDeltas = map (\f -> (fillFeeAsset f, - fillFee f)) (roundFills rr)
    in M.fromListWith (+) (baseDeltas ++ feeDeltas)
