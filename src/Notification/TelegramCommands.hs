{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Notification.TelegramCommands
    ( runCommandListener
    ) where

import Bot.Config (Config(..))
import Bot.Domain (Asset(..), RoundResult(..), RoundStatus(..), PersistedRound(..))
import Bot.Runtime (BotState(..))
import Exchange.Interface (Exchange(..))
import Text.Read (readMaybe)
import Notification.Telegram (sendTelegramMessage)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON(..), genericParseJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Numeric (showFFloat)
import qualified Data.Text as T


data TgChat = TgChat { tgChatId :: Int } deriving (Show, Generic)

instance FromJSON TgChat where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \f -> if f == "tgChatId" then "id" else f }

data TgMessage = TgMessage
    { tgMsgText :: Maybe T.Text
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


fetchUpdates :: Config -> Int -> IO [TgUpdate]
fetchUpdates config offset = runReq defaultHttpConfig $ do
    let url = https "api.telegram.org"
                /: ("bot" <> cfgTelegramToken config)
                /: "getUpdates"
    resp <- req GET url NoReqBody jsonResponse $
               "offset"  =: offset <>
               "timeout" =: (25 :: Int)
    let body = responseBody resp :: TgUpdatesResponse
    return $ if tgRespOk body then tgRespResult body else []


dispatchCommand :: Exchange e => e -> Config -> BotState -> UTCTime -> T.Text -> Int -> IO ()
dispatchCommand exchange config st startTime cmd chatId = do
    now <- getCurrentTime
    let parts   = T.words (T.dropWhile (== '/') cmd)
        rawCmd  = if null parts then "" else head parts
        cmdWord = T.map toLower . T.takeWhile (/= '@') $ rawCmd
        argN    = case parts of
                    (_:arg:_) -> case readMaybe (T.unpack arg) of
                                    Just x  -> max 1 (min 20 x)
                                    Nothing -> 5
                    _         -> 5
        cfgForChat = config { cfgTelegramChatId = T.pack (show chatId) }
    reply <- case cmdWord of
                    "balance"     -> fmtBalanceReply exchange -- Consulta por red y ya devuelvo IO T.Text
                    "status"      -> return $ fmtStatus st startTime now
                    "pnl"         -> return $ fmtPnl (bsPnlAccumulated st) (bsRoundCount st)
                    "open_orders" -> return $ fmtOpenOrders (length (bsOpenOrders st))
                    "history"     -> return $ fmtHistory (bsTradeHistory st) argN
                    _             -> return "Comandos:\n/balance\n/status\n/pnl\n/open_orders\n/history [N]"
    sendTelegramMessage cfgForChat reply >>= \case
        Left err -> putStrLn $ "Error respondiendo comando Telegram: " ++ show err
        Right _  -> pure ()


runCommandListener :: Exchange e => e -> Config -> IORef BotState -> UTCTime -> IO ()
runCommandListener exchange config stateRef startTime = go 0
  where
    go lastId = do
        updates <- fetchUpdates config (lastId + 1)
                     `catch` \(_ :: SomeException) -> return []
        mapM_ (handleUpdate exchange config stateRef startTime) updates
        let nextId = maximum (lastId : map tgUpdateId updates)
        go nextId

handleUpdate :: Exchange e => e -> Config -> IORef BotState -> UTCTime -> TgUpdate -> IO ()
handleUpdate exchange config stateRef startTime upd =
    case tgUpdateMessage upd of
        Nothing  -> pure ()
        Just msg -> case tgMsgText msg of
            Nothing  -> pure ()
            Just txt -> do
                let cid = tgChatId (tgMsgChat msg)
                botState <- readIORef stateRef
                dispatchCommand exchange config botState startTime txt cid


fmtBalanceReply :: Exchange e => e -> IO T.Text
fmtBalanceReply exchange =
    either (const "Error obteniendo balance del exchange.") fmtBalance
        <$> fetchBalances exchange

fmtBalance :: Map Asset Double -> T.Text
fmtBalance bals
    | M.null bals = "Sin datos de balance (esperando primer ciclo)."
    | otherwise   = T.unlines $ "Balances actuales:" : map fmtEntry (M.toList bals)
  where
    fmtEntry (asset, qty) =
        "  " <> T.pack (show asset) <> ": " <> fixed (assetDecimals asset) qty

fmtStatus :: BotState -> UTCTime -> UTCTime -> T.Text
fmtStatus st startTime now =
    T.unlines
        [ "Estado del bot"
        , "Rondas ejecutadas: " <> T.pack (show (bsRoundCount st))
        , "Ultimo resultado:  " <> fmtLastRound (bsLastRoundResult st)
        , "Ordenes abiertas:  " <> T.pack (show (length (bsOpenOrders st)))
        , "Uptime:            " <> fmtUptime (diffUTCTime now startTime)
        ]

fmtLastRound :: Maybe RoundResult -> T.Text
fmtLastRound Nothing   = "sin datos"
fmtLastRound (Just rr) = case roundStatus rr of
    RoundSuccess      -> "exitosa"
    RoundFailed msg   -> "fallida (" <> msg <> ")"
    RoundPartial errs -> "parcial (" <> T.pack (show (length errs)) <> " errores)"

fmtPnl :: Map Asset Double -> Int -> T.Text
fmtPnl pnl rounds
    | M.null pnl = "Sin PnL registrado aun."
    | otherwise  = T.unlines $
        ("PnL acumulado (" <> T.pack (show rounds) <> " rondas):") : map fmtEntry (M.toList pnl)
  where
    fmtEntry (asset, qty) =
        let sign = if qty >= 0 then "+" else ""
        in "  " <> T.pack (show asset) <> ": " <> sign <> fixed (assetDecimals asset) qty

fmtOpenOrders :: Int -> T.Text
fmtOpenOrders 0 = "Sin ordenes abiertas."
fmtOpenOrders n = T.pack (show n) <> " orden(es) en curso."

fmtHistory :: [PersistedRound] -> Int -> T.Text
fmtHistory [] _ = "Sin operaciones registradas aun."
fmtHistory rounds n =
    let recent  = reverse (take n (reverse rounds))
        shown   = min n (length rounds)
        hdr     = "Ultimas " <> T.pack (show shown) <> " operacion(es):"
        entries = zipWith fmtEntry [1..] recent
    in T.unlines $ hdr : "" : intercalate [""] entries
  where
    fmtEntry i pr =
        [ T.pack (show (i :: Int)) <> ". [" <> prStatus pr <> "]"
        , "   Ruta:    " <> prettifyPairs (prPairs pr)
        , "   Entrada: " <> fixed 2 (prAmountIn pr)  <> " USDT"
        , "   Salida:  " <> fixed 2 (prAmountOut pr) <> " USDT"
        , "   PnL:     " <> signedFixed (prPnlUsdt pr) <> " USDT"
        ]

prettifyPairs :: T.Text -> T.Text
prettifyPairs s = T.intercalate " \x2192 " (map parseSingle (T.splitOn " -> " s))
  where
    parseSingle p = case T.words p of
        (_:_:_:b:_:_:q:_) -> T.filter (/= ',') b <> "/" <> T.filter (/= '}') q
        _                   -> p

signedFixed :: Double -> T.Text
signedFixed x
    | x >= 0    = "+" <> fixed 2 x
    | otherwise =        fixed 2 x

fmtUptime :: (RealFrac a) => a -> T.Text
fmtUptime secs =
    let total = floor secs :: Int
        h = total `div` 3600
        m = (total `mod` 3600) `div` 60
        s = total `mod` 60
    in T.pack $ show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

fixed :: Int -> Double -> T.Text
fixed d x = T.pack $ showFFloat (Just d) x ""

assetDecimals :: Asset -> Int
assetDecimals USDT = 2
assetDecimals _    = 8
