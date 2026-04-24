{-# LANGUAGE DeriveGeneric #-}

module Bot.Persist
  ( PersistedState(..)
  , loadState
  , saveState
  , fromBotState
  , applyToInitialState
  ) where

import Bot.Domain (Asset, PersistedRound)
import Bot.Runtime (BotState(..), initialBotState)
import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import System.IO.Error (isDoesNotExistError)

data PersistedState = PersistedState
  { psRoundCount     :: Int
  , psPnlAccumulated :: Map Asset Double
  , psTradeHistory   :: [PersistedRound]
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

    empty = PersistedState { psRoundCount = 0, psPnlAccumulated = mempty, psTradeHistory = [] }

saveState :: FilePath -> PersistedState -> IO ()
saveState = encodeFile

historyLimit :: Int
historyLimit = 100

fromBotState :: BotState -> PersistedState
fromBotState st = PersistedState
  { psRoundCount     = bsRoundCount st
  , psPnlAccumulated = bsPnlAccumulated st
  , psTradeHistory   = takeRight historyLimit (bsTradeHistory st)
  }

applyToInitialState :: PersistedState -> BotState
applyToInitialState ps = initialBotState
  { bsRoundCount     = psRoundCount ps
  , bsPnlAccumulated = psPnlAccumulated ps
  , bsTradeHistory   = psTradeHistory ps
  }

takeRight :: Int -> [a] -> [a]
takeRight n xs = drop (length xs - n) xs
