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
import qualified Data.Map.Strict as M

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
        p1   = arbPair1 path
        p2   = arbPair2 path
        p3   = arbPair3 path
        perc = fixed 2 (arbProfitPerc opp)
        absP = fmtAmount (qtyAsset amountIn) (arbProfitAbs opp) ++ " " ++ show (qtyAsset amountIn)
        amountIn = arbAmountIn opp
        amountOut = arbAmountOut opp
    in unlines
        [ "🔥 Oportunidad de arbitraje detectada"
        , ""
        , "Ruta:"
        , "  1) " ++ fmtPair p1
        , "  2) " ++ fmtPair p2
        , "  3) " ++ fmtPair p3
        , "  Ciclo: " ++ fmtPair p1 ++ " → " ++ fmtPair p2 ++ " → " ++ fmtPair p3
        , ""
        , "Ganancia estimada:"
        , "  " ++ perc ++ "% (" ++ absP ++ ")"
        , ""
        , "Entrada:"
        , "  " ++ fmtAmount (qtyAsset amountIn) (qtyAmount amountIn) ++ " " ++ show (qtyAsset amountIn)
        , ""
        , "Salida esperada:"
        , "  " ++ fmtAmount (qtyAsset amountOut) (qtyAmount amountOut) ++ " " ++ show (qtyAsset amountOut)
        ]

formatRoundResult :: RoundResult -> String
formatRoundResult rr =
    let statusLine = "Estado: " ++ show (roundStatus rr)
        pnlStartLine = "PnL neto (moneda inicio): " ++ showNetPnlStart rr
        pnlLine    = "PnL neto (USDT): " ++ showNetPnl rr
        pnlPctLine =
            "PnL: " ++ showNetPnlPercent rr
        operationSizeLine = "Tamaño operación: " ++ showOperationSize rr
        deltasBlock = formatAssetDeltasBlock rr
        stepsBlock  = case roundFills rr of
            []    -> "Pasos:\n  • sin ejecuciones"
            fills -> "Pasos (" ++ show (length fills) ++ "):\n" ++ intercalate "\n\n" (zipWith formatFillLine [1 :: Int ..] fills)
    in unlines
        [ "📈 Resultado post-trade"
        , ""
        , statusLine
        , ""
        , pnlStartLine
        , pnlLine
        , pnlPctLine
        , ""
        , operationSizeLine
        , ""
        , deltasBlock
        , ""
        , stepsBlock
        ]

showNetPnl :: RoundResult -> String
showNetPnl rr =
    signedAmountByAsset USDT (roundNetPnlUsdt rr) ++ " USDT"

showNetPnlStart :: RoundResult -> String
showNetPnlStart rr =
    let a = qtyAsset (roundAmountIn rr)
    in signedAmountByAsset a (roundNetPnlStart rr) ++ " " ++ show a

showOperationSize :: RoundResult -> String
showOperationSize rr =
    fmtAmount (qtyAsset amtIn) (qtyAmount amtIn) ++ " " ++ show (qtyAsset amtIn)
  where
    amtIn = roundAmountIn rr

showNetPnlPercent :: RoundResult -> String
showNetPnlPercent rr
    | opSize <= 0 = "N/A"
    | otherwise   = signedPercent pct
  where
    opSize = qtyAmount (roundAmountIn rr)
    pct = 100 * roundNetPnlStart rr / opSize

formatAssetDeltasBlock :: RoundResult -> String
formatAssetDeltasBlock rr
    | M.null deltas = "Cambio activos:\n  • sin cambios"
    | otherwise     = unlines ("Cambio activos:" : map showEntry (M.toList deltas))
  where
    deltas = roundAssetDeltas rr
    showEntry (asset, amount) = "  • " ++ signedAmountByAsset asset amount ++ " " ++ show asset

roundAssetDeltas :: RoundResult -> M.Map Asset Double
roundAssetDeltas rr =
    let amtIn = roundAmountIn rr
        amtOut = roundAmountOut rr
        baseDeltas =
            [ (qtyAsset amtIn, - qtyAmount amtIn)
            , (qtyAsset amtOut, qtyAmount amtOut)
            ]
        feeDeltas = map (\f -> (fillFeeAsset f, - fillFee f)) (roundFills rr)
    in M.fromListWith (+) (baseDeltas ++ feeDeltas)

formatExecutionError :: String -> String
formatExecutionError err = unlines
    [ "📉 Resultado post-trade"
    , "Estado: RoundFailed"
    , "Error: " ++ err
    ]

formatFillLine :: Int -> Fill -> String
formatFillLine idx fill =
    "  " ++ show idx ++ ". "
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

signedPercent :: Double -> String
signedPercent p
    | p >= 0     = "+" ++ fixed 2 p ++ "%"
    | otherwise  = fixed 2 p ++ "%"

fmtPair :: Pair -> String
fmtPair pair = show (base pair) ++ "/" ++ show (quote pair)

