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
  ) where

import Bot.Config (Config)
import Bot.Domain (RoundResult)
import Binance.API.Instance (BinanceExchange)
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

data Env = Env
  { envConfig   :: Config
  , envExchange :: BinanceExchange
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

newtype BotM a = BotM
  { unBotM :: ReaderT Env (StateT BotState (ExceptT BotError IO)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState BotState, MonadError BotError, MonadIO)

runBotM :: Env -> BotState -> BotM a -> IO (Either BotError (a, BotState))
runBotM env st (BotM m) =
  runExceptT $
    (\x -> (x,)) <$> evalStateT (runReaderT m env) st

