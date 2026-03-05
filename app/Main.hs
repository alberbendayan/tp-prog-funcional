{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Config
import Binance.API.Client
import Binance.API.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    config <- loadConfig
    let baseUrl = cfgBaseUrl config
    
    pingResult <- ping baseUrl
    case pingResult of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "Conectividad OK"
    
    
    putStrLn "Obteniendo precio de BTCUSDT..."
    priceResult <- getPrice baseUrl "BTCUSDT"
    case priceResult of
        Left err -> putStrLn $ "Error: " ++ show err
        Right tickerPrice -> do
            let symbol = unSymbol $ tpSymbol tickerPrice
            let price = unPrice $ tpPrice tickerPrice
            TIO.putStrLn $ symbol <> " = $" <> T.pack (show price)