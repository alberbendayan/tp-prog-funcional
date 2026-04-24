{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Config
import Bot.Domain
import Bot.Arbitraje
import Bot.Runtime
import Bot.Persist
import Exchange.Interface
import Exchange.AppExchange (AppExchange, configureAppExchange)
import Notification.Telegram
import Notification.TelegramCommands (runCommandListener)
import Control.Concurrent         (forkIO, threadDelay)
import Control.Monad              (when)
import Control.Monad.Error.Class  (catchError)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Reader       (asks)
import Control.Monad.State.Strict (get, put)
import Data.IORef                 (IORef, newIORef, writeIORef)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Time.Clock            (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as M

tradingAssets :: [Asset]
tradingAssets = [BTC, ETH, BNB, USDT]

-- | Intenta construir y validar el plan de ejecuci?n a partir de una oportunidad.
-- Devuelve Left con un mensaje de error si alguno de los pasos falla.
buildValidPlan :: MarketSnapshot -> ArbOpportunity -> Either String ExecutionPlan
buildValidPlan snapshot opp =
    case opportunityToExecutionPlan snapshot opp of
        Nothing   -> Left "No se pudo construir el plan de ejecuci?n"
        Just plan -> case validateAndQuantizePlan plan of
            Left planErr -> Left $ "Plan inv?lido tras cuantizaci?n: " ++ show planErr
            Right valid  -> case validatePlanLiquidity snapshot valid of
                Left liqErr -> Left $ "Liquidez insuficiente para ejecutar: " ++ liqErr
                Right ()    -> Right valid

-- | Imprime el resultado de una ronda de trading.
printRoundResult :: RoundResult -> IO ()
printRoundResult rr = do
    putStrLn $ "Status: " ++ show (roundStatus rr)
    putStrLn $ "PnL neto operatoria: " ++ show (roundNetPnlUsdt rr) ++ " USDT"

formatBotStateSummary :: BotState -> String
formatBotStateSummary st =
    unlines
      [ "Estado del bot"
      , "- Rondas ejecutadas: " ++ show (bsRoundCount st)
      , "- Ultima ronda registrada: " ++ show (hasLastRoundResult st)
      , "- Errores por ronda: " ++ show (bsErrorsPerRound st)
      , "- PnL acumulado: " ++ formatAssetMap (bsPnlAccumulated st)
      , "- Balances (delta): " ++ formatAssetMap (bsBalances st)
      , "- Ordenes abiertas modeladas: " ++ show (length (bsOpenOrders st))
      ]

formatAssetMap :: Map Asset Double -> String
formatAssetMap mp
  | M.null mp  = "sin datos"
  | otherwise  = intercalate ", " $ map formatEntry (M.toList mp)
  where
    formatEntry (asset, amount) = show amount ++ " " ++ show asset

hasLastRoundResult :: BotState -> Bool
hasLastRoundResult st = case bsLastRoundResult st of
  Nothing -> False
  Just _  -> True

-- | Ejecuta la decisi?n del bot.
-- Toma el estado actual y devuelve el reporte y el nuevo estado.
executeDecision :: Exchange e => Config -> e -> MarketSnapshot -> Decision -> BotState -> IORef BotState -> UTCTime -> IO (Maybe String, BotState)
executeDecision _      _        _        NoTrade       st _        _         = return (Nothing, st)
executeDecision config exchange snapshot (DoTrade opp) st stateRef startTime =
    case buildValidPlan snapshot opp of
        Left msg        -> do
            putStrLn msg
            return (Just (formatExecutionError msg), st)
        Right validPlan -> do
            let env = Env { envConfig = config, envExchange = exchange
                          , envStateRef = stateRef, envStartTime = startTime }
            result <- runBotM env st (executeRound snapshot validPlan)
            case result of
                Left err -> do
                    let msg = "Error ejecutando ronda: " ++ show err
                    putStrLn msg
                    return (Just (formatExecutionError (show err)), st)
                Right (rr, newSt) -> do
                    printRoundResult rr
                    putStrLn (formatBotStateSummary newSt)
                    return (Just (formatRoundResult rr), newSt)

-- | Orquesta un ciclo completo: detecci?n, decisi?n, ejecuci?n y notificaci?n.
-- Devuelve el nuevo estado del bot para ser persistido.
resolveTradeAmount :: Config -> Either ExchangeError (Map Asset Double) -> Double
resolveTradeAmount config (Right bals) = min (cfgMaxTradeUSDT config) (M.findWithDefault 0 USDT bals)
resolveTradeAmount config (Left _)     = cfgMaxTradeUSDT config

logBalanceResult :: Either ExchangeError (Map Asset Double) -> Double -> IO ()
logBalanceResult (Left err) _   = putStrLn $ "Warning: no se pudo obtener balance: " ++ show err
logBalanceResult (Right bals) amt =
    putStrLn $ "Balance USDT disponible: " ++ show (M.findWithDefault 0 USDT bals)
            ++ " | Monto a operar: " ++ show amt

mkPersistedRound :: RoundResult -> IO PersistedRound
mkPersistedRound rr = do
    now <- getCurrentTime
    let ts     = case roundFills rr of { (f:_) -> fillTime f; [] -> now }
        pairs  = intercalate " -> " (map (show . fillPair) (roundFills rr))
        amtIn  = qtyAmount (roundAmountIn rr)
        amtOut = qtyAmount (roundAmountOut rr)
        pnl    = roundNetPnlUsdt rr
        status = case roundStatus rr of
                    RoundSuccess    -> "exitosa"
                    RoundFailed _   -> "fallida"
                    RoundPartial _  -> "parcial"
    return PersistedRound
        { prTimestamp = ts
        , prPairs     = if null pairs then "N/A" else pairs
        , prAmountIn  = amtIn
        , prAmountOut = amtOut
        , prPnlUsdt   = pnl
        , prStatus    = status
        }

handleSnapshot :: Config -> AppExchange -> MarketSnapshot -> BotState -> IORef BotState -> UTCTime -> IO BotState
handleSnapshot config exchange snapshot st stateRef startTime = do
    balancesResult  <- fetchBalances exchange
    let effectiveAmount = resolveTradeAmount config balancesResult
    logBalanceResult balancesResult effectiveAmount
    let paths    = allTriangularPaths tradingAssets
        amountIn = AssetQty USDT effectiveAmount
        opps     = detectOpportunities paths snapshot amountIn
        decision = makeDecision (cfgMinProfit config) opps
    putStrLn $ "\n" ++ formatDecision decision
    (postTradeReport, newSt) <- executeDecision config exchange snapshot decision st stateRef startTime
    let fetchedBals = either (const M.empty) id balancesResult
    newSt' <- case bsLastRoundResult newSt of
        Nothing -> return newSt { bsLastFetchedBalances = fetchedBals }
        Just rr -> do
            pr <- mkPersistedRound rr
            let history = bsTradeHistory newSt ++ [pr]
                trimmed = drop (max 0 (length history - 100)) history
            return newSt { bsLastFetchedBalances = fetchedBals, bsTradeHistory = trimmed }
    when (cfgTelegramEnabled config) $ do
        sendTelegramMessage config (formatDecision decision) >>=
            either
                (\err -> putStrLn $ "Error enviando Telegram: " ++ show err)
                (\_ -> putStrLn "Notificaci?n de decisi?n enviada a Telegram")
        case postTradeReport of
            Nothing -> return ()
            Just report ->
                sendTelegramMessage config report >>=
                    either
                        (\err -> putStrLn $ "Error enviando Telegram post-trade: " ++ show err)
                        (\_ -> putStrLn "Notificaci?n post-trade enviada a Telegram")
    return newSt'

runOneRound :: BotM AppExchange ()
runOneRound = do
    checkConnectivityOrThrow
    liftIO $ putStrLn "Conectividad OK"
    snapshot  <- fetchMarketSnapshotOrThrow tradingAssets
    config    <- asks envConfig
    exchange  <- asks envExchange
    stateRef  <- asks envStateRef
    startTime <- asks envStartTime
    curSt     <- get
    newSt     <- liftIO $ handleSnapshot config exchange snapshot curSt stateRef startTime
    liftIO $ saveState (cfgStateFile config) (fromBotState newSt)
    liftIO $ writeIORef stateRef newSt
    put newSt

botLoop :: BotM AppExchange ()
botLoop = do
    runOneRound `catchError` \err ->
        liftIO $ putStrLn $ "Error critico (continuando): " ++ show err
    config <- asks envConfig
    liftIO $ threadDelay (cfgPollInterval config * 1000000)
    botLoop

main :: IO ()
main = do
    config    <- loadConfig
    exchange  <- configureAppExchange config
    persisted <- loadState (cfgStateFile config)
    startTime <- getCurrentTime
    let initSt = applyToInitialState persisted
    stateRef  <- newIORef initSt
    let env = Env { envConfig    = config
                  , envExchange  = exchange
                  , envStateRef  = stateRef
                  , envStartTime = startTime }
    when (cfgTelegramEnabled config) $
        forkIO (runCommandListener config stateRef startTime) >> pure ()
    result <- runBotM env initSt botLoop
    either (\err -> putStrLn $ "Error irrecuperable: " ++ show err) (\_ -> pure ()) result
