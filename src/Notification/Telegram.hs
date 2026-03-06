{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Notification.Telegram
    ( sendTelegramMessage
    , formatOpportunityTelegram
    ) where

import Notification.Types
import Binance.API.Types
import Bot.Config
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
    -- mempty :: Option 'Https y representa las opciones vacias
    
    return (responseBody response :: TelegramResponse)

handleTelegramResult :: Either SomeException TelegramResponse -> Either TelegramError ()
handleTelegramResult (Left e) = Left $ TelegramSendError (show e)
handleTelegramResult (Right resp)
    | ok resp   = Right ()
    | otherwise = Left $ TelegramSendError "Telegram API returned ok=false"

formatOpportunityTelegram :: [TickerPrice] -> String
formatOpportunityTelegram prices =
    let msgHeader = unlines
            [ "🚨 Notificacion de Precios"
            , "=========================="
            , ""
            ]
        body = unlines $ map formatSinglePrice prices
        footer = unlines
            [ ""
            , "Bot de arbitraje @arbitrin."
            ]
    in msgHeader ++ body ++ footer

formatSinglePrice :: TickerPrice -> String
formatSinglePrice tickerPrice =
    let symbol = T.unpack $ unSymbol $ tpSymbol tickerPrice
        price = unPrice $ tpPrice tickerPrice
    in "  " ++ symbol ++ ": $" ++ show price
