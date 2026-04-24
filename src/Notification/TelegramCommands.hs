{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Notification.TelegramCommands
    ( runCommandListener
    ) where

import Bot.Config (Config(..))
import Bot.Domain (Asset(..), RoundResult(..), RoundStatus(..))
import Bot.Runtime (BotState(..))
import Notification.Telegram (sendTelegramMessage)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON(..), genericParseJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Numeric (showFFloat)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Tipos JSON de Telegram
-- ---------------------------------------------------------------------------

data TgChat = TgChat { tgChatId :: Int } deriving (Show, Generic)

instance FromJSON TgChat where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \f -> if f == "tgChatId" then "id" else f }

data TgMessage = TgMessage
    { tgMsgText :: Maybe String
    , tgMsgChat :: TgChat
    } deriving (Show, Generic)

instance FromJSON TgMessage where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \f -> case f of
            "tgMsgText" -> "text"
            "tgMsgChat" -> "chat"
            x           -> x }

data TgUpdate = TgUpdate
    { tgUpdateId      :: Int
    , tgUpdateMessage :: Maybe TgMessage
    } deriving (Show, Generic)

instance FromJSON TgUpdate where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \f -> case f of
            "tgUpdateId"      -> "update_id"
            "tgUpdateMessage" -> "message"
            x                 -> x }

data TgUpdatesResponse = TgUpdatesResponse
    { tgRespOk     :: Bool
    , tgRespResult :: [TgUpdate]
    } deriving (Show, Generic)

instance FromJSON TgUpdatesResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \f -> case f of
            "tgRespOk"     -> "ok"
            "tgRespResult" -> "result"
            x              -> x }

-- ---------------------------------------------------------------------------
-- Polling
-- ---------------------------------------------------------------------------

fetchUpdates :: Config -> Int -> IO [TgUpdate]
fetchUpdates config offset = runReq defaultHttpConfig $ do
    let url = https "api.telegram.org"
                /: ("bot" <> T.pack (cfgTelegramToken config))
                /: "getUpdates"
    resp <- req GET url NoReqBody jsonResponse $
               "offset"  =: offset <>
               "timeout" =: (25 :: Int)
    let body = responseBody resp :: TgUpdatesResponse
    return $ if tgRespOk body then tgRespResult body else []

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

dispatchCommand :: Config -> BotState -> UTCTime -> String -> Int -> IO ()
dispatchCommand config st startTime cmd chatId = do
    now <- getCurrentTime
    let cmd' = map toLower . takeWhile (/= ' ') . dropWhile (== '/') $ cmd
        reply = case cmd' of
                    "balance"     -> fmtBalance (bsLastFetchedBalances st)
                    "status"      -> fmtStatus st startTime now
                    "pnl"         -> fmtPnl (bsPnlAccumulated st) (bsRoundCount st)
                    "open_orders" -> fmtOpenOrders (length (bsOpenOrders st))
                    _             -> "Comandos disponibles: /balance /status /pnl /open_orders"
        cfgForChat = config { cfgTelegramChatId = show chatId }
    sendTelegramMessage cfgForChat reply >>= \case
        Left err -> putStrLn $ "Error respondiendo comando Telegram: " ++ show err
        Right _  -> pure ()

-- ---------------------------------------------------------------------------
-- Loop del listener
-- ---------------------------------------------------------------------------

runCommandListener :: Config -> IORef BotState -> UTCTime -> IO ()
runCommandListener config stateRef startTime = go 0
  where
    go lastId = do
        updates <- fetchUpdates config (lastId + 1)
                     `catch` \(_ :: SomeException) -> return []
        mapM_ (handleUpdate config stateRef startTime) updates
        let nextId = foldl (\acc u -> max acc (tgUpdateId u)) lastId updates
        go nextId

handleUpdate :: Config -> IORef BotState -> UTCTime -> TgUpdate -> IO ()
handleUpdate config stateRef startTime upd =
    case tgUpdateMessage upd of
        Nothing  -> pure ()
        Just msg -> case tgMsgText msg of
            Nothing  -> pure ()
            Just txt -> do
                let cid = tgChatId (tgMsgChat msg)
                putStrLn $ "Chat ID recibido: " ++ show cid
                st <- readIORef stateRef
                dispatchCommand config st startTime txt cid

-- ---------------------------------------------------------------------------
-- Formatters
-- ---------------------------------------------------------------------------

fmtBalance :: Map Asset Double -> String
fmtBalance bals
    | M.null bals = "Sin datos de balance (esperando primer ciclo)."
    | otherwise   = unlines $ "Balances actuales:" : map fmtEntry (M.toList bals)
  where
    fmtEntry (asset, qty) = "  " ++ show asset ++ ": " ++ fixed (assetDecimals asset) qty

fmtStatus :: BotState -> UTCTime -> UTCTime -> String
fmtStatus st startTime now =
    unlines
        [ "Estado del bot"
        , "Rondas ejecutadas: " ++ show (bsRoundCount st)
        , "Ultimo resultado:  " ++ fmtLastRound (bsLastRoundResult st)
        , "Ordenes abiertas:  " ++ show (length (bsOpenOrders st))
        , "Uptime:            " ++ fmtUptime (diffUTCTime now startTime)
        ]

fmtLastRound :: Maybe RoundResult -> String
fmtLastRound Nothing   = "sin datos"
fmtLastRound (Just rr) = case roundStatus rr of
    RoundSuccess     -> "exitosa"
    RoundFailed msg  -> "fallida (" ++ msg ++ ")"
    RoundPartial errs -> "parcial (" ++ show (length errs) ++ " errores)"

fmtPnl :: Map Asset Double -> Int -> String
fmtPnl pnl rounds
    | M.null pnl = "Sin PnL registrado aun."
    | otherwise  = unlines $
        ("PnL acumulado (" ++ show rounds ++ " rondas):") : map fmtEntry (M.toList pnl)
  where
    fmtEntry (asset, qty) =
        let sign = if qty >= 0 then "+" else ""
        in "  " ++ show asset ++ ": " ++ sign ++ fixed (assetDecimals asset) qty

fmtOpenOrders :: Int -> String
fmtOpenOrders 0 = "Sin ordenes abiertas."
fmtOpenOrders n = show n ++ " orden(es) en curso."

fmtUptime :: (RealFrac a) => a -> String
fmtUptime secs =
    let total = floor secs :: Int
        h = total `div` 3600
        m = (total `mod` 3600) `div` 60
        s = total `mod` 60
    in show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

fixed :: Int -> Double -> String
fixed d x = showFFloat (Just d) x ""

assetDecimals :: Asset -> Int
assetDecimals USDT = 2
assetDecimals _    = 8
