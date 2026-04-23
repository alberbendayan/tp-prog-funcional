{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Notification.Telegram
    ( sendTelegramMessage
    , formatDecision
    , formatRoundResult
    , formatExecutionError
    ) where

import Notification.Types
import Bot.Config
import Bot.Domain
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import Data.List (intercalate)
import Numeric (showFFloat)

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
        , "Entrada: " ++ show (qtyAmount (arbAmountIn opp)) ++ " " ++ show (qtyAsset (arbAmountIn opp))
        , "Salida esperada: " ++ show (qtyAmount (arbAmountOut opp)) ++ " " ++ show (qtyAsset (arbAmountOut opp))
        ]

formatRoundResult :: RoundResult -> String
formatRoundResult rr =
    let statusLine = "Estado: " ++ show (roundStatus rr)
        pnlLine    = "PnL: " ++ either ("error - " ++) showPnl (roundPnl rr)
        fillsLine  = case roundFills rr of
            []    -> "Fills: sin ejecuciones"
            fills -> "Fills (" ++ show (length fills) ++ "):\n" ++ intercalate "\n" (zipWith formatFillLine [1 :: Int ..] fills)
    in unlines
        [ "📈 Resultado post-trade"
        , statusLine
        , pnlLine
        , fillsLine
        ]

formatExecutionError :: String -> String
formatExecutionError err = unlines
    [ "📉 Resultado post-trade"
    , "Estado: RoundFailed"
    , "Error: " ++ err
    ]

showPnl :: AssetQty -> String
showPnl pnl = signedAmountByAsset (qtyAsset pnl) (qtyAmount pnl) ++ " " ++ show (qtyAsset pnl)

formatFillLine :: Int -> Fill -> String
formatFillLine idx fill =
    show idx ++ ". "
        ++ show (fillSide fill) ++ " "
        ++ fmtAmount (base (fillPair fill)) (fillAmountBase fill) ++ " "
        ++ show (base (fillPair fill)) ++ " | precio: "
        ++ fmtPrice (quote (fillPair fill)) (unPrice (fillPrice fill))
        ++ " " ++ show (quote (fillPair fill))
        ++ " (fee: " ++ fmtAmount (fillFeeAsset fill) (fillFee fill) ++ " " ++ show (fillFeeAsset fill) ++ ")"

fmtPrice :: Asset -> Double -> String
fmtPrice asset p = fixed (decimalsForAsset asset) p

fmtAmount :: Asset -> Double -> String
fmtAmount asset a = fixed (decimalsForAsset asset) a

signedAmountByAsset :: Asset -> Double -> String
signedAmountByAsset asset a
    | a > 0     = "+" ++ fixed decimals a
    | otherwise = fixed decimals a
  where
    decimals = decimalsForAsset asset

decimalsForAsset :: Asset -> Int
decimalsForAsset USDT = 2
decimalsForAsset _    = 8

fixed :: Int -> Double -> String
fixed decimals x = showFFloat (Just decimals) x ""

