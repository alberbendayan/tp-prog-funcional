{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Config
import Bot.Domain
import Bot.Arbitraje
import Binance.API.Client
import Binance.API.Types
import Binance.API.Conversion
import Notification.Telegram
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)

-- | Monedas que queremos monitorear
-- Solo definís las monedas, los pares se generan automáticamente
tradingAssets :: [Asset]
tradingAssets = [BTC, ETH, BNB, USDT]

main :: IO ()
main = do
    config <- loadConfig
    let baseUrl = cfgBaseUrl config

    pingResult <- ping baseUrl
    case pingResult of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "Conectividad OK"

    let allPairs = generateAllPairs tradingAssets

    tickers <- fetchBookTickersForPairs baseUrl allPairs

    displayBookTickers tickers

    let commission  = CommissionRate (cfgCommissionRate config)
        snapshot    = buildMarketSnapshot tickers commission
        paths       = allTriangularPaths tradingAssets
        amountIn    = cfgMaxTradeUSDT config
        opps        = detectOpportunities paths snapshot amountIn
        decision    = makeDecision (cfgMinProfit config) opps

    putStrLn $ "\n" ++ formatDecision decision

    when (cfgTelegramEnabled config) $ do
        telegramResult <- sendTelegramMessage config (formatDecision decision)
        case telegramResult of
            Left err -> putStrLn $ "Error enviando Telegram: " ++ show err
            Right _  -> putStrLn "Notificacion de Telegram enviada exitosamente"

displayBookTickers :: [BookTicker] -> IO ()
displayBookTickers [] = putStrLn "No se pudo obtener ningun ticker"
displayBookTickers tickers = mapM_ displaySingleBookTicker tickers

displaySingleBookTicker :: BookTicker -> IO ()
displaySingleBookTicker bt = do
    let symbol = unSymbol $ btSymbol bt
    let bid = unPrice $ btBidPrice bt
    let ask = unPrice $ btAskPrice bt
    let spread = ask - bid
    let spreadPerc = (spread / bid) * 100
    TIO.putStrLn $ symbol <> " | Bid: $" <> T.pack (show bid)
                          <> " | Ask: $" <> T.pack (show ask)
                          <> " | Spread: " <> T.pack (show spreadPerc) <> "%"
