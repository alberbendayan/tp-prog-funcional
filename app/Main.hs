{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Config
import Bot.Domain
import Bot.Arbitraje
import Bot.Runtime
import Exchange.Interface
import Exchange.AppExchange (AppExchange, configureAppExchange)
import Notification.Telegram
import Control.Monad (when)

tradingAssets :: [Asset]
tradingAssets = [BTC, ETH, BNB, USDT]

-- | Intenta construir y validar el plan de ejecución a partir de una oportunidad.
-- Devuelve Left con un mensaje de error si alguno de los pasos falla.
buildValidPlan :: MarketSnapshot -> ArbOpportunity -> Either String ExecutionPlan
buildValidPlan snapshot opp =
    case opportunityToExecutionPlan snapshot opp of
        Nothing   -> Left "No se pudo construir el plan de ejecución"
        Just plan -> case validateAndQuantizePlan plan of
            Left planErr -> Left $ "Plan inválido tras cuantización: " ++ show planErr
            Right valid  -> Right valid

-- | Formatea el PnL de una ronda para mostrarlo por pantalla.
formatPnl :: Either String AssetQty -> String
formatPnl (Left msg)  = "error — " ++ msg
formatPnl (Right pnl) = show (qtyAmount pnl) ++ " " ++ show (qtyAsset pnl)

-- | Imprime el resultado de una ronda de trading.
printRoundResult :: RoundResult -> IO ()
printRoundResult rr = do
    putStrLn $ "Status: " ++ show (roundStatus rr)
    putStrLn $ "PnL:    " ++ formatPnl (roundPnl rr)

-- | Ejecuta la decisión del bot.
-- NoTrade no produce ningún efecto. DoTrade construye, valida y ejecuta el plan.
executeDecision :: Exchange e => Config -> e -> MarketSnapshot -> Decision -> IO (Maybe String)
executeDecision _      _        _        NoTrade       = return Nothing
executeDecision config exchange snapshot (DoTrade opp) =
    case buildValidPlan snapshot opp of
        Left msg        -> do
            putStrLn msg
            return $ Just (formatExecutionError msg)
        Right validPlan -> do
            let env = Env { envConfig = config, envExchange = exchange }
            result <- runBotM env initialBotState (executeRound validPlan)
            either
                (\err -> do
                    let msg = "Error ejecutando ronda: " ++ show err
                    putStrLn msg
                    return $ Just (formatExecutionError (show err)))
                (\(rr, _) -> do
                    printRoundResult rr
                    return $ Just (formatRoundResult rr))
                result

-- | Orquesta un ciclo completo: detección, decisión, ejecución y notificación.
handleSnapshot :: Config -> AppExchange -> MarketSnapshot -> IO ()
handleSnapshot config exchange snapshot = do
    let paths    = allTriangularPaths tradingAssets
        amountIn = AssetQty USDT (cfgMaxTradeUSDT config)
        opps     = detectOpportunities paths snapshot amountIn
        decision = makeDecision (cfgMinProfit config) opps
    putStrLn $ "\n" ++ formatDecision decision
    postTradeReport <- executeDecision config exchange snapshot decision
    when (cfgTelegramEnabled config) $
        do
            sendTelegramMessage config (formatDecision decision) >>=
                either
                    (\err -> putStrLn $ "Error enviando Telegram: " ++ show err)
                    (\_ -> putStrLn "Notificación de decisión enviada a Telegram")
            case postTradeReport of
                Nothing -> return ()
                Just report ->
                    sendTelegramMessage config report >>=
                        either
                            (\err -> putStrLn $ "Error enviando Telegram post-trade: " ++ show err)
                            (\_ -> putStrLn "Notificación post-trade enviada a Telegram")

runWithExchange :: Config -> AppExchange -> IO ()
runWithExchange config exchange = do
    checkConnectivity exchange >>=
        either
            (\err -> putStrLn $ "Error de conectividad: " ++ show err)
            (\_ -> putStrLn "Conectividad OK")
    fetchMarketSnapshot exchange tradingAssets >>=
        either
            (\err -> putStrLn $ "Error obteniendo mercado: " ++ show err)
            (handleSnapshot config exchange)

main :: IO ()
main = do
    config <- loadConfig
    exchange <- configureAppExchange config
    runWithExchange config exchange
