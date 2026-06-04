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
import Numeric (showFFloat)
import qualified Data.Map.Strict as M

data TelegramRequest = TelegramRequest
    { chat_id :: T.Text
    , text :: T.Text
    } deriving (Show, Generic)

instance ToJSON TelegramRequest where
    toJSON = genericToJSON defaultOptions

data TelegramResponse = TelegramResponse
    { ok :: Bool
    } deriving (Show, Generic)

instance FromJSON TelegramResponse

sendTelegramMessage :: Config -> T.Text -> IO (Either TelegramError ())
sendTelegramMessage config message = do
    result <- try $ sendTelegramRequest config message
    return $ handleTelegramResult result

buildTelegramUrl :: T.Text -> Url 'Https
buildTelegramUrl token =
    https "api.telegram.org"
        /: "bot" <> token
        /: "sendMessage"

sendTelegramRequest :: Config -> T.Text -> IO TelegramResponse
sendTelegramRequest config message = do
    let token  = cfgTelegramToken config
        chatId = cfgTelegramChatId config
        url    = buildTelegramUrl token
        payload = TelegramRequest
            { chat_id = chatId
            , text    = message
            }
    response <- runReq defaultHttpConfig $
        req POST url (ReqBodyJson payload) jsonResponse mempty
    return (responseBody response :: TelegramResponse)

handleTelegramResult :: Either SomeException TelegramResponse -> Either TelegramError ()
handleTelegramResult (Left e) = Left $ TelegramSendError (T.pack $ show e)
handleTelegramResult (Right resp)
    | ok resp   = Right ()
    | otherwise = Left $ TelegramSendError "Telegram API returned ok=false"

formatDecision :: Decision -> T.Text
formatDecision NoTrade = "Sin oportunidades de arbitraje rentables."
formatDecision (DoTrade opp) =
    let path      = arbPath opp
        p1        = arbPair1 path
        p2        = arbPair2 path
        p3        = arbPair3 path
        perc      = fixed 2 (unProfitPct (arbProfitPerc opp))
        amountIn  = arbAmountIn opp
        amountOut = arbAmountOut opp
        absP      = fmtAmount (qtyAsset amountIn) (arbProfitAbs opp)
                 <> " " <> T.pack (show (qtyAsset amountIn))
    in T.unlines
        [ "🔥 Oportunidad de arbitraje detectada"
        , ""
        , "Ruta:"
        , "  1) " <> fmtPair p1
        , "  2) " <> fmtPair p2
        , "  3) " <> fmtPair p3
        , "  Ciclo: " <> fmtPair p1 <> " → " <> fmtPair p2 <> " → " <> fmtPair p3
        , ""
        , "Ganancia estimada:"
        , "  " <> perc <> "% (" <> absP <> ")"
        , ""
        , "Entrada:"
        , "  " <> fmtAmount (qtyAsset amountIn) (qtyAmount amountIn)
               <> " " <> T.pack (show (qtyAsset amountIn))
        , ""
        , "Salida esperada:"
        , "  " <> fmtAmount (qtyAsset amountOut) (qtyAmount amountOut)
               <> " " <> T.pack (show (qtyAsset amountOut))
        ]

formatRoundResult :: RoundResult -> T.Text
formatRoundResult rr =
    let statusLine        = "Estado: " <> T.pack (show (roundStatus rr))
        pnlStartLine      = "PnL neto (moneda inicio): " <> showNetPnlStart rr
        pnlLine           = "PnL neto (USDT): " <> showNetPnl rr
        pnlPctLine        = "PnL: " <> showNetPnlPercent rr
        operationSizeLine = "Tamaño operación: " <> showOperationSize rr
        deltasBlock       = formatAssetDeltasBlock rr
        stepsBlock        = case roundFills rr of
            []    -> "Pasos:\n  • sin ejecuciones"
            fills -> "Pasos (" <> T.pack (show (length fills)) <> "):\n"
                  <> T.intercalate "\n\n" (zipWith formatFillLine [1 :: Int ..] fills)
    in T.unlines
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

showNetPnl :: RoundResult -> T.Text
showNetPnl rr =
    signedAmountByAsset USDT (roundNetPnlUsdt rr) <> " USDT"

showNetPnlStart :: RoundResult -> T.Text
showNetPnlStart rr =
    let a = qtyAsset (roundAmountIn rr)
    in signedAmountByAsset a (roundNetPnlStart rr) <> " " <> T.pack (show a)

showOperationSize :: RoundResult -> T.Text
showOperationSize rr =
    fmtAmount (qtyAsset amtIn) (qtyAmount amtIn) <> " " <> T.pack (show (qtyAsset amtIn))
  where
    amtIn = roundAmountIn rr

showNetPnlPercent :: RoundResult -> T.Text
showNetPnlPercent rr
    | opSize <= 0 = "N/A"
    | otherwise   = signedPercent pct
  where
    opSize = qtyAmount (roundAmountIn rr)
    pct    = 100 * roundNetPnlStart rr / opSize

formatAssetDeltasBlock :: RoundResult -> T.Text
formatAssetDeltasBlock rr
    | M.null deltas = "Cambio activos:\n  • sin cambios"
    | otherwise     = T.unlines ("Cambio activos:" : map showEntry (M.toList deltas))
  where
    deltas = roundAssetDeltas rr
    showEntry (asset, amount) =
        "  • " <> signedAmountByAsset asset amount <> " " <> T.pack (show asset)

roundAssetDeltas :: RoundResult -> M.Map Asset Double
roundAssetDeltas rr =
    let amtIn  = roundAmountIn rr
        amtOut = roundAmountOut rr
        baseDeltas =
            [ (qtyAsset amtIn,  - qtyAmount amtIn)
            , (qtyAsset amtOut,   qtyAmount amtOut)
            ]
        feeDeltas = map (\f -> (fillFeeAsset f, - fillFee f)) (roundFills rr)
    in M.fromListWith (+) (baseDeltas ++ feeDeltas)

formatExecutionError :: T.Text -> T.Text
formatExecutionError err = T.unlines
    [ "📉 Resultado post-trade"
    , "Estado: RoundFailed"
    , "Error: " <> err
    ]

formatFillLine :: Int -> Fill -> T.Text
formatFillLine idx fill =
    "  " <> T.pack (show idx) <> ". "
        <> T.pack (show (fillSide fill)) <> " "
        <> fmtAmount (base (fillPair fill)) (fillAmountBase fill) <> " "
        <> T.pack (show (base (fillPair fill))) <> " | precio: "
        <> fmtPrice (quote (fillPair fill)) (unPrice (fillPrice fill))
        <> " " <> T.pack (show (quote (fillPair fill)))
        <> " (fee: " <> fmtAmount (fillFeeAsset fill) (fillFee fill)
        <> " " <> T.pack (show (fillFeeAsset fill)) <> ")"

fmtPrice :: Asset -> Double -> T.Text
fmtPrice asset p = fixed (decimalsForAsset asset) p

fmtAmount :: Asset -> Double -> T.Text
fmtAmount asset a = fixed (decimalsForAsset asset) a

signedAmountByAsset :: Asset -> Double -> T.Text
signedAmountByAsset asset a
    | a > 0     = "+" <> fixed decimals a
    | otherwise = fixed decimals a
  where
    decimals = decimalsForAsset asset

decimalsForAsset :: Asset -> Int
decimalsForAsset USDT = 2
decimalsForAsset _    = 8

fixed :: Int -> Double -> T.Text
fixed decimals x = T.pack $ showFFloat (Just decimals) x ""

signedPercent :: Double -> T.Text
signedPercent p
    | p >= 0    = "+" <> fixed 2 p <> "%"
    | otherwise =        fixed 2 p <> "%"

fmtPair :: Pair -> T.Text
fmtPair pair = T.pack (show (base pair)) <> "/" <> T.pack (show (quote pair))
