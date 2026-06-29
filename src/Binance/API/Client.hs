{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Binance.API.Client
    ( ping
    , getBookTicker
    , getTradeFees
    , getAccountInfo
    , placeMarketOrder
    , BinanceError(..)
    ) where

import Binance.API.Types
import Binance.API.Endpoints
import qualified Binance.API.Auth as Auth
import Control.Exception (Exception, try, SomeException)
import Data.Aeson (FromJSON)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Proxy (Proxy)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Req

data BinanceError
    = NetworkError Text
    | ParseError Text
    | APIError Int Text
    deriving (Show, Eq)

instance Exception BinanceError

posixTimestampMs :: IO Text
posixTimestampMs = T.pack . show . (floor :: Double -> Integer) . (* 1000) . realToFrac <$> getPOSIXTime

signedTimestampParams :: Text -> IO [(Text, Text)]
signedTimestampParams apiSecret = do
    ts <- posixTimestampMs
    let sig = Auth.signQueryString apiSecret ("timestamp=" <> ts)
    return [("timestamp", ts), ("signature", sig)]

marketQtyParam :: MarketOrderQty -> (Text, Text)
marketQtyParam (QtyBase  q) = ("quantity",      T.pack $ show q)
marketQtyParam (QtyQuote q) = ("quoteOrderQty", T.pack $ show q)

ping :: Text -> IO (Either BinanceError Bool)
ping baseUrl = fmap (const True) <$> makeGetRequest baseUrl pingEndpoint [] ignoreResponse

getBookTicker :: Text -> Text -> IO (Either BinanceError BookTicker)
getBookTicker baseUrl symbol =
    makeJsonGetRequest baseUrl bookTickerEndpoint [("symbol", symbol)]

getAccountInfo :: Text -> Text -> Text -> IO (Either BinanceError AccountInfo)
getAccountInfo baseUrl apiKey apiSecret = do
    params <- signedTimestampParams apiSecret
    makeAuthJsonGetRequest baseUrl accountEndpoint params apiKey

getTradeFees :: Text -> Text -> Text -> IO (Either BinanceError [TradeFee])
getTradeFees baseUrl apiKey apiSecret = do
    params <- signedTimestampParams apiSecret
    makeAuthJsonGetRequest baseUrl tradeFeeEndpoint params apiKey

placeMarketOrder
    :: Text -> Text -> Text -> Text -> Text -> MarketOrderQty
    -> IO (Either BinanceError OrderResponse)
placeMarketOrder baseUrl apiKey apiSecret symbol side marketQty = do
    ts <- posixTimestampMs
    let (qtyKey, qtyVal) = marketQtyParam marketQty
        queryStr = "symbol=" <> symbol <> "&side=" <> side <> "&type=MARKET"
                <> "&" <> qtyKey <> "=" <> qtyVal <> "&timestamp=" <> ts
        sig    = Auth.signQueryString apiSecret queryStr
        params = [("symbol", symbol), ("side", side), ("type", "MARKET"),
                  (qtyKey, qtyVal), ("timestamp", ts), ("signature", sig)]
    makeAuthJsonPostRequest baseUrl orderEndpoint params apiKey

wrapReq :: IO a -> IO (Either BinanceError a)
wrapReq action = do
    result <- try action
    return $ case result of
        Left (e :: SomeException) -> Left $ NetworkError (T.pack $ show e)
        Right r                   -> Right r

buildAuthHeader :: Text -> Option 'Https
buildAuthHeader apiKey = header "X-MBX-APIKEY" (TE.encodeUtf8 apiKey)

runApiRequest
    :: (HttpMethod method, HttpResponse response, HttpBodyAllowed (AllowsBody method) 'NoBody)
    => method -> Text -> Text -> [(Text, Text)] -> Option 'Https -> Proxy response
    -> IO (Either BinanceError response)
runApiRequest method baseUrl endpoint params extraOpts responseType =
    wrapReq $ runReq defaultHttpConfig $
        let (url, reqParams) = mkRequest baseUrl endpoint params
        in req method url NoReqBody responseType (reqParams <> extraOpts)

makeGetRequest :: HttpResponse response
               => Text -> Text -> [(Text, Text)] -> Proxy response -> IO (Either BinanceError response)
makeGetRequest baseUrl endpoint params = runApiRequest GET baseUrl endpoint params mempty

makeAuthGetRequest :: HttpResponse response
                   => Text -> Text -> [(Text, Text)] -> Text -> Proxy response -> IO (Either BinanceError response)
makeAuthGetRequest baseUrl endpoint params apiKey = runApiRequest GET baseUrl endpoint params (buildAuthHeader apiKey)

makeAuthPostRequest :: HttpResponse response
                    => Text -> Text -> [(Text, Text)] -> Text -> Proxy response -> IO (Either BinanceError response)
makeAuthPostRequest baseUrl endpoint params apiKey = runApiRequest POST baseUrl endpoint params (buildAuthHeader apiKey)

makeJsonGetRequest :: FromJSON a
                   => Text -> Text -> [(Text, Text)]
                   -> IO (Either BinanceError a)
makeJsonGetRequest baseUrl endpoint params =
    fmap responseBody <$> makeGetRequest baseUrl endpoint params jsonResponse

makeAuthJsonGetRequest :: FromJSON a
                       => Text -> Text -> [(Text, Text)] -> Text
                       -> IO (Either BinanceError a)
makeAuthJsonGetRequest baseUrl endpoint params apiKey =
    fmap responseBody <$> makeAuthGetRequest baseUrl endpoint params apiKey jsonResponse

makeAuthJsonPostRequest :: FromJSON a
                        => Text -> Text -> [(Text, Text)] -> Text
                        -> IO (Either BinanceError a)
makeAuthJsonPostRequest baseUrl endpoint params apiKey =
    fmap responseBody <$> makeAuthPostRequest baseUrl endpoint params apiKey jsonResponse

extractHost :: Text -> Text
extractHost = T.drop (T.length "https://")

extractPathParts :: Text -> [Text]
extractPathParts = filter (not . T.null) . T.splitOn "/"

buildUrl :: Text -> Text -> Url 'Https
buildUrl baseUrl endpoint = foldl' (/:) (https (extractHost baseUrl)) (extractPathParts endpoint)

buildParams :: [(Text, Text)] -> Option 'Https
buildParams = mconcat . map (uncurry (=:))
-- (=:) :: QueryParam param => Text -> Text -> param
mkRequest :: Text -> Text -> [(Text, Text)] -> (Url 'Https, Option 'Https)
mkRequest baseUrl endpoint params = (buildUrl baseUrl endpoint, buildParams params)
