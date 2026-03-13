{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Notification.Telegram
    ( sendTelegramMessage
    , formatBookTickersTelegram
    , formatDecision
    ) where

import Notification.Types
import Binance.API.Types
import Bot.Config
import Bot.Domain
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req

data TelegramRequest = TelegramRequest
    { chat_id :: String
    , text :: String
    } deriving (Show, Generic)

instance ToJSON TelegramRequest where
    toJSON = genericToJSON defaultOptions

data TelegramResponse = TelegramResponse
    { ok :: Bool
    } deriving (Show, Generic)

instance FromJSON TelegramResponse

sendTelegramMessage :: Config -> String -> IO (Either TelegramError ())
sendTelegramMessage config message = do
    result <- try $ sendTelegramRequest config message
    return $ handleTelegramResult result

buildTelegramUrl :: String -> Url 'Https
buildTelegramUrl token = 
    https "api.telegram.org" 
        /: "bot" <> T.pack token 
        /: "sendMessage"

sendTelegramRequest :: Config -> String -> IO TelegramResponse
sendTelegramRequest config message = do
    let token = cfgTelegramToken config
    let chatId = cfgTelegramChatId config
    let url = buildTelegramUrl token
    let payload = TelegramRequest
            { chat_id = chatId
            , text = message
            }
    
    response <- runReq defaultHttpConfig $ do
        req POST url (ReqBodyJson payload) jsonResponse mempty
    
    return (responseBody response :: TelegramResponse)

handleTelegramResult :: Either SomeException TelegramResponse -> Either TelegramError ()
handleTelegramResult (Left e) = Left $ TelegramSendError (show e)
handleTelegramResult (Right resp)
    | ok resp   = Right ()
    | otherwise = Left $ TelegramSendError "Telegram API returned ok=false"

formatBookTickersTelegram :: [BookTicker] -> String
formatBookTickersTelegram tickers =
    let msgHeader = unlines
            [ "📊 Book Tickers (Bid/Ask)"
            , "=========================="
            , ""
            ]
        body = unlines $ map formatSingleBookTicker tickers
        footer = unlines
            [ ""
            , "Bot de arbitraje @arbitrin."
            ]
    in msgHeader ++ body ++ footer

formatDecision :: Decision -> String
formatDecision NoTrade = "Sin oportunidades de arbitraje rentables."
formatDecision (DoTrade opp) =
    let path = arbPath opp
        p1   = show (arbPair1 path)
        p2   = show (arbPair2 path)
        p3   = show (arbPair3 path)
        perc = arbProfitPerc opp
        absP = arbProfitAbs opp
    in unlines
        [ "Oportunidad: " ++ p1 ++ " -> " ++ p2 ++ " -> " ++ p3
        , "Ganancia: " ++ show perc ++ "% (" ++ show absP ++ " USDT)"
        , "Entrada: " ++ show (arbAmountIn opp)
        , "Salida esperada: " ++ show (arbAmountOut opp)
        ]

formatSingleBookTicker :: BookTicker -> String
formatSingleBookTicker bt =
    let symbol = T.unpack $ unSymbol $ btSymbol bt
        bid = unPrice $ btBidPrice bt
        ask = unPrice $ btAskPrice bt
        spread = ask - bid
        spreadPerc = (spread / bid) * 100
    in "  " ++ symbol ++ ":\n" 
       ++ "    Bid: $" ++ show bid ++ "\n"
       ++ "    Ask: $" ++ show ask ++ "\n"
       ++ "    Spread: " ++ show spreadPerc ++ "%"
