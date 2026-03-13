{-# LANGUAGE OverloadedStrings #-}

module Bot.Config
    ( Config(..)
    , loadConfig
    ) where

import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

data Config = Config
    { cfgApiKey          :: String
    , cfgApiSecret       :: String
    , cfgBaseUrl         :: String
    , cfgMinProfit       :: Double
    , cfgMaxTradeUSDT    :: Double
    , cfgCommissionRate  :: Double
    , cfgTelegramToken   :: String
    , cfgTelegramChatId  :: String
    , cfgTelegramEnabled :: Bool
    } deriving (Show)

loadConfig :: IO Config
loadConfig = do
    _ <- loadFile defaultConfig
    
    apiKey       <- getEnvOrDefault "BINANCE_API_KEY" "" id
    apiSecret    <- getEnvOrDefault "BINANCE_API_SECRET" "" id
    baseUrl      <- getEnvOrDefault "BINANCE_BASE_URL" "https://testnet.binance.vision" id
    minProfit       <- getEnvOrDefault "BOT_MIN_PROFIT_PERCENTAGE" 0.5 read
    maxTradeUSDT    <- getEnvOrDefault "BOT_MAX_TRADE_AMOUNT_USDT" 100.0 read
    commissionRate  <- getEnvOrDefault "BOT_COMMISSION_RATE" 0.001 read

    telegramToken <- getEnvOrDefault "TELEGRAM_BOT_TOKEN" "" id
    telegramChatId <- getEnvOrDefault "TELEGRAM_CHAT_ID" "" id
    telegramEnabled <- getEnvOrDefault "TELEGRAM_ENABLED" True readBool
    
    return Config
        { cfgApiKey = apiKey
        , cfgApiSecret = apiSecret
        , cfgBaseUrl = baseUrl
        , cfgMinProfit = minProfit
        , cfgMaxTradeUSDT   = maxTradeUSDT
        , cfgCommissionRate = commissionRate
        , cfgTelegramToken = telegramToken
        , cfgTelegramChatId = telegramChatId
        , cfgTelegramEnabled = telegramEnabled
        }

readBool :: String -> Bool
readBool "true" = True
readBool "True" = True
readBool "1" = True
readBool _ = False

getEnvOrDefault :: String -> b -> (String -> b) -> IO b
getEnvOrDefault key def converter = fmap (maybe def converter) (lookupEnv key)

