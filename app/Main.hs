{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Config
import Bot.Domain
import Bot.Arbitraje
import Exchange.Interface
import Binance.API.Instance
import Binance.API.Types (Asset(..))
import Notification.Telegram
import Control.Monad (when)

tradingAssets :: [Asset]
tradingAssets = [BTC, ETH, BNB, USDT]

main :: IO ()
main = do
    config <- loadConfig
    let exchange = BinanceExchange
            { binanceBaseUrl           = cfgBaseUrl config
            , binanceDefaultCommission = CommissionRate (cfgCommissionRate config)
            , binanceApiKey            = cfgApiKey config
            , binanceApiSecret         = cfgApiSecret config
            }

    pingResult <- checkConnectivity exchange
    case pingResult of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _  -> putStrLn "Conectividad OK"

    snapshotResult <- fetchMarketSnapshot exchange tradingAssets
    case snapshotResult of
        Left err -> putStrLn $ "Error obteniendo mercado: " ++ show err
        Right snapshot -> do
            let paths    = allTriangularPaths tradingAssets
                amountIn = AssetQty USDT (cfgMaxTradeUSDT config)
                opps     = detectOpportunities paths snapshot amountIn
                decision = makeDecision (cfgMinProfit config) opps

            putStrLn $ "\n" ++ formatDecision decision

            when (cfgTelegramEnabled config) $ do
                telegramResult <- sendTelegramMessage config (formatDecision decision)
                case telegramResult of
                    Left err -> putStrLn $ "Error enviando Telegram: " ++ show err
                    Right _  -> putStrLn "Notificacion de Telegram enviada exitosamente"
