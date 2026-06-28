{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Config
import Bot.Domain
import Bot.Arbitraje
import Bot.Runtime
import Bot.Persist
import Bot.Pricing (assetUsdtRateWhenSelling)
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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

tradingAssets :: [Asset]
tradingAssets = [BTC, ETH, BNB, USDT]

buildValidPlan :: MarketSnapshot -> ArbOpportunity -> Either T.Text ExecutionPlan
buildValidPlan snapshot opp =
    case opportunityToExecutionPlan snapshot opp of
        Nothing   -> Left "No se pudo construir el plan de ejecución"
        Just plan -> case validateAndQuantizePlan plan of
            Left planErr -> Left $ "Plan inválido tras cuantización: " <> T.pack (show planErr)
            Right valid  -> case validatePlanLiquidity snapshot valid of
                Left liqErr -> Left $ "Liquidez insuficiente para ejecutar: " <> liqErr
                Right ()    -> Right valid

printRoundResult :: RoundResult -> IO ()
printRoundResult rr = do
    putStrLn $ "Status: " ++ show (roundStatus rr)
    putStrLn $
        "PnL neto (moneda inicio): "
            ++ show (roundNetPnlStart rr)
            ++ " "
            ++ show (qtyAsset (roundAmountIn rr))
    putStrLn $ "PnL neto (USDT): " ++ show (roundNetPnlUsdt rr)

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

executeDecision :: Exchange e => Config -> e -> MarketSnapshot -> Decision -> BotState -> IORef BotState -> UTCTime -> IO (Maybe T.Text, BotState)
executeDecision _      _        _        NoTrade       st _        _         = return (Nothing, st)
executeDecision config exchange snapshot (DoTrade opp) st stateRef startTime =
    case buildValidPlan snapshot opp of
        Left msg        -> do
            TIO.putStrLn msg
            return (Just (formatExecutionError msg), st)
        Right validPlan -> do
            let env = Env { envConfig = config, envExchange = exchange
                          , envStateRef = stateRef, envStartTime = startTime }
            result <- runBotM env st (executeRound snapshot validPlan)
            case result of
                Left err -> do
                    let msg = "Error ejecutando ronda: " <> T.pack (show err)
                    TIO.putStrLn msg
                    return (Just (formatExecutionError msg), st)
                Right (rr, newSt) -> do
                    printRoundResult rr
                    putStrLn (formatBotStateSummary newSt)
                    return (Just (formatRoundResult rr), newSt)


buildCandidateAmounts :: MarketSnapshot -> Double -> Map Asset Double -> [AssetQty]
buildCandidateAmounts snapshot maxUsdtNotional bals =
    [ AssetQty asset qty
    | asset <- tradingAssets
    , let bal = M.findWithDefault 0 asset bals
    , bal > 1e-12
    , Right rate <- [assetUsdtRateWhenSelling snapshot asset]
    , unUsdtRate rate > 0
    , let qtyMaxNotional = maxUsdtNotional / unUsdtRate rate
    , let qty = min bal qtyMaxNotional
    , qty > 1e-12
    ]

logCandidateAmounts :: [AssetQty] -> IO ()
logCandidateAmounts [] =
    putStrLn "Montos candidatos: ninguno (sin balance o sin cotización a USDT)."
logCandidateAmounts xs =
    putStrLn $ "Montos candidatos (tope notional USDT por activo): " ++ intercalate ", " (map fmtQty xs)
  where
    fmtQty (AssetQty a q) = show q ++ " " ++ show a

logBalanceResult :: Either ExchangeError (Map Asset Double) -> IO ()
logBalanceResult (Left err) =
    putStrLn $ "Warning: no se pudo obtener balance: " ++ show err
logBalanceResult (Right bals) =
    putStrLn $ "Balances: " ++ formatAssetMap bals

mkPersistedRound :: RoundResult -> IO PersistedRound
mkPersistedRound rr = do
    now <- getCurrentTime
    let ts     = case roundFills rr of { (f:_) -> fillTime f; [] -> now }
        pairs  = T.intercalate " -> " (map (T.pack . show . fillPair) (roundFills rr))
        amtIn  = qtyAmount (roundAmountIn rr)
        amtOut = qtyAmount (roundAmountOut rr)
        pnl    = roundNetPnlUsdt rr
        status :: T.Text
        status = case roundStatus rr of
                    RoundSuccess   -> "exitosa"
                    RoundFailed _  -> "fallida"
                    RoundPartial _ -> "parcial"
    return PersistedRound
        { prTimestamp = ts
        , prPairs     = if null (roundFills rr) then "N/A" else pairs
        , prAmountIn  = amtIn
        , prAmountOut = amtOut
        , prPnlUsdt   = pnl
        , prStatus    = status
        }

computeDecision :: Config -> MarketSnapshot -> [AssetQty] -> Decision
computeDecision config snapshot candidates =
    let paths = allTriangularPaths tradingAssets
        opps  = concatMap (detectOpportunities paths snapshot) candidates
    in makeDecision (cfgMinProfit config) opps

recordRound :: Map Asset Double -> BotState -> IO BotState
recordRound fetchedBals newSt =
    case bsLastRoundResult newSt of
        Nothing -> return newSt { bsLastFetchedBalances = fetchedBals }
        Just rr -> do
            pr <- mkPersistedRound rr
            let history = bsTradeHistory newSt ++ [pr]
                trimmed = drop (max 0 (length history - 100)) history
            return newSt { bsLastFetchedBalances = fetchedBals, bsTradeHistory = trimmed }

notifyTelegram :: Config -> Decision -> Maybe T.Text -> IO ()
notifyTelegram config decision postTradeReport =
    when (cfgTelegramEnabled config) $ do
        send (formatDecision decision) "de decisión"
        case postTradeReport of
            Nothing     -> return ()
            Just report -> send report "post-trade"
  where
    send msg label =
        sendTelegramMessage config msg >>=
            either
                (\err -> putStrLn $ "Error enviando Telegram " ++ label ++ ": " ++ show err)
                (\_   -> putStrLn $ "Notificación " ++ label ++ " enviada a Telegram")

handleSnapshot :: Config -> AppExchange -> MarketSnapshot -> BotState -> IORef BotState -> UTCTime -> IO BotState
handleSnapshot config exchange snapshot st stateRef startTime = do
    balancesResult <- fetchBalances exchange
    logBalanceResult balancesResult
    let bals       = either (const M.empty) id balancesResult
        candidates = buildCandidateAmounts snapshot (cfgMaxTradeUSDT config) bals
    logCandidateAmounts candidates
    let decision = computeDecision config snapshot candidates
    TIO.putStrLn $ "\n" <> formatDecision decision
    (postTradeReport, newSt) <- executeDecision config exchange snapshot decision st stateRef startTime
    newSt' <- recordRound bals newSt
    notifyTelegram config decision postTradeReport
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
        forkIO (runCommandListener exchange config stateRef startTime) >> pure ()
    result <- runBotM env initSt botLoop
    either (\err -> putStrLn $ "Error irrecuperable: " ++ show err) (\_ -> pure ()) result
