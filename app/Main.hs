{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Config
import Binance.API.Client
import Binance.API.Types
import Notification.Telegram
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)

main :: IO ()
main = do
    config <- loadConfig
    let baseUrl = cfgBaseUrl config
    
    pingResult <- ping baseUrl
    case pingResult of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "Conectividad OK"
    
    
    putStrLn "Obteniendo precios de BTC, ETH y BNB..."
    
    btcResult <- getPrice baseUrl "BTCUSDT"
    ethResult <- getPrice baseUrl "ETHUSDT"
    bnbResult <- getPrice baseUrl "BNBUSDT"
    
    let allResults = [btcResult, ethResult, bnbResult]
    let successfulPrices = collectSuccessfulPrices allResults
    
    displayPrices successfulPrices
    notifyViaTelegram config successfulPrices

collectSuccessfulPrices :: [Either BinanceError TickerPrice] -> [TickerPrice]
collectSuccessfulPrices results = 
    [price | Right price <- results]

displayPrices :: [TickerPrice] -> IO ()
displayPrices [] = putStrLn "No se pudo obtener ningun precio"
displayPrices prices = mapM_ displaySinglePrice prices

displaySinglePrice :: TickerPrice -> IO ()
displaySinglePrice tickerPrice = do
    let symbol = unSymbol $ tpSymbol tickerPrice
    let price = unPrice $ tpPrice tickerPrice
    TIO.putStrLn $ symbol <> " = $" <> T.pack (show price)

notifyViaTelegram :: Config -> [TickerPrice] -> IO ()
notifyViaTelegram _ [] = return ()
notifyViaTelegram config prices = do
    when (cfgTelegramEnabled config) $ do
        putStrLn "Enviando notificacion por Telegram..."
        let telegramMsg = formatOpportunityTelegram prices
        telegramResult <- sendTelegramMessage config telegramMsg
        case telegramResult of
            Left err -> putStrLn $ "Error enviando Telegram: " ++ show err
            Right _ -> putStrLn "Notificacion de Telegram enviada exitosamente"
