{-# LANGUAGE DeriveGeneric #-}

module Bot.Persist
  ( PersistedState(..)
  , loadState
  , saveState
  , fromBotState
  , applyToInitialState
  ) where

import Bot.Domain (Asset)
import Bot.Runtime (BotState(..), initialBotState)
import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import System.IO.Error (isDoesNotExistError)

data PersistedState = PersistedState
  { psRoundCount     :: Int
  , psPnlAccumulated :: Map Asset Double
  } deriving (Show, Eq, Generic)

instance FromJSON PersistedState
instance ToJSON PersistedState

loadState :: FilePath -> IO PersistedState
loadState path =
    (eitherDecodeFileStrict path >>= handleResult) `catch` handleIOError
  where
    handleResult (Right ps) = return ps
    handleResult (Left err) = do
        putStrLn $ "Advertencia: estado persistido inválido (" ++ err ++ "), arrancando desde cero"
        return empty

    handleIOError :: IOException -> IO PersistedState
    handleIOError e
      | isDoesNotExistError e = return empty
      | otherwise = do
          putStrLn $ "Advertencia: no se pudo leer el archivo de estado (" ++ show e ++ "), arrancando desde cero"
          return empty

    empty = PersistedState { psRoundCount = 0, psPnlAccumulated = mempty }

saveState :: FilePath -> PersistedState -> IO ()
saveState = encodeFile

fromBotState :: BotState -> PersistedState
fromBotState st = PersistedState
  { psRoundCount     = bsRoundCount st
  , psPnlAccumulated = bsPnlAccumulated st
  }

applyToInitialState :: PersistedState -> BotState
applyToInitialState ps = initialBotState
  { bsRoundCount     = psRoundCount ps
  , bsPnlAccumulated = psPnlAccumulated ps
  }
