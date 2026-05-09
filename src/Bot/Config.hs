{-# LANGUAGE OverloadedStrings #-}

module Bot.Config
    ( ExchangeKind(..)
    , Config(..)
    , loadConfig
    ) where

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

data ExchangeKind
    = ExchangeKindBinance
    | ExchangeKindFake
    deriving (Show, Eq)

readExchangeKind :: String -> ExchangeKind
readExchangeKind s =
    case map toLower s of
        "fake"    -> ExchangeKindFake
        "binance" -> ExchangeKindBinance
        _         -> ExchangeKindBinance

data Config = Config
    { cfgApiKey          :: Text
    , cfgApiSecret       :: Text
    , cfgBaseUrl         :: Text
    , cfgMinProfit       :: Double
    , cfgMaxTradeUSDT    :: Double
    , cfgCommissionRate  :: Double
    , cfgExchangeKind    :: ExchangeKind
    , cfgTelegramToken   :: Text
    , cfgTelegramChatId  :: Text
    , cfgTelegramEnabled :: Bool
    , cfgStateFile       :: FilePath
    , cfgPollInterval    :: Int
    } deriving (Show)

loadConfig :: IO Config
loadConfig = do
    _ <- loadFile defaultConfig

    apiKey       <- getEnvOrDefault "BINANCE_API_KEY" T.empty T.pack
    apiSecret    <- getEnvOrDefault "BINANCE_API_SECRET" T.empty T.pack
    baseUrl      <- getEnvOrDefault "BINANCE_BASE_URL" "https://testnet.binance.vision" T.pack
    minProfit       <- getEnvOrDefault "BOT_MIN_PROFIT_PERCENTAGE" 0.5 read
    maxTradeUSDT    <- getEnvOrDefault "BOT_MAX_TRADE_AMOUNT_USDT" 100.0 read
    commissionRate  <- getEnvOrDefault "BOT_COMMISSION_RATE" 0.001 read

    telegramToken  <- getEnvOrDefault "TELEGRAM_BOT_TOKEN" T.empty T.pack
    telegramChatId <- getEnvOrDefault "TELEGRAM_CHAT_ID" T.empty T.pack
    telegramEnabled <- getEnvOrDefault "TELEGRAM_ENABLED" True readBool
    exchangeRaw     <- getEnvOrDefault "BOT_EXCHANGE" "binance" id
    stateFile       <- getEnvOrDefault "BOT_STATE_FILE" "bot_state.json" id
    pollInterval    <- getEnvOrDefault "BOT_POLL_INTERVAL" 30 read

    return Config
        { cfgApiKey = apiKey
        , cfgApiSecret = apiSecret
        , cfgBaseUrl = baseUrl
        , cfgMinProfit = minProfit
        , cfgMaxTradeUSDT   = maxTradeUSDT
        , cfgCommissionRate = commissionRate
        , cfgExchangeKind   = readExchangeKind exchangeRaw
        , cfgTelegramToken = telegramToken
        , cfgTelegramChatId = telegramChatId
        , cfgTelegramEnabled = telegramEnabled
        , cfgStateFile = stateFile
        , cfgPollInterval = pollInterval
        }

readBool :: String -> Bool
readBool "true" = True
readBool "True" = True
readBool "1"    = True
readBool _      = False

getEnvOrDefault :: String -> b -> (String -> b) -> IO b
getEnvOrDefault key def converter = fmap (maybe def converter) (lookupEnv key)
