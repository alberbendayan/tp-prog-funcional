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
  , executeRound
  ) where

import Bot.Config (Config)
import Bot.Domain
import Exchange.Interface (Exchange(..))
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.State.Strict (StateT, MonadState, runStateT, modify)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

data Env e = Env
  { envConfig   :: Config
  , envExchange :: e
  }

data BotState = BotState
  { bsRounds :: [RoundResult]
  }

initialBotState :: BotState
initialBotState = BotState
  { bsRounds = []
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

executeStepsSequentially :: (MonadIO m, Exchange e) => e -> [OrderStep] -> [Fill] -> m ([Fill], [String])
executeStepsSequentially _  []     acc = return (reverse acc, [])
executeStepsSequentially ex (s:ss) acc = executeOrder ex s >>= onResult
  where
    onResult (Left  err)  = return (reverse acc, [show err])
    onResult (Right fill) = executeStepsSequentially ex ss (fill : acc)

executeRound :: Exchange e => ExecutionPlan -> BotM e RoundResult
executeRound plan = do
    env <- ask
    let steps = executionPlanSteps plan
        amtIn = extractStepInputAmount (head steps)
    (fills, errs) <- executeStepsSequentially (envExchange env) steps []
    let result = buildRoundResult amtIn fills errs
    modify $ \s -> s { bsRounds = bsRounds s ++ [result] }
    return result

