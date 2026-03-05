{-# LANGUAGE OverloadedStrings #-}

module Bot.Config
    ( Config(..)
    , loadConfig
    ) where

import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

data Config = Config
    { cfgApiKey       :: String
    , cfgApiSecret    :: String
    , cfgBaseUrl      :: String
    , cfgMinProfit    :: Double
    , cfgMaxTradeUSDT :: Double
    } deriving (Show)

loadConfig :: IO Config
loadConfig = do
    _ <- loadFile defaultConfig
    
    apiKey       <- getEnvOrDefault "BINANCE_API_KEY" "" id
    apiSecret    <- getEnvOrDefault "BINANCE_API_SECRET" "" id
    baseUrl      <- getEnvOrDefault "BINANCE_BASE_URL" "https://testnet.binance.vision" id
    minProfit    <- getEnvOrDefault "BOT_MIN_PROFIT_PERCENTAGE" 0.5 read
    maxTradeUSDT <- getEnvOrDefault "BOT_MAX_TRADE_AMOUNT_USDT" 100.0 read
    
    return Config
        { cfgApiKey = apiKey
        , cfgApiSecret = apiSecret
        , cfgBaseUrl = baseUrl
        , cfgMinProfit = minProfit
        , cfgMaxTradeUSDT = maxTradeUSDT
        }

getEnvOrDefault :: String -> b -> (String -> b) -> IO b
getEnvOrDefault key def converter = fmap (maybe def converter) (lookupEnv key)

