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

ping :: Text -> IO (Either BinanceError Bool)
ping baseUrl = do
    result <- makeGetRequest baseUrl pingEndpoint [] ignoreResponse
    case result of
        Left err -> return $ Left err
        Right _  -> return $ Right True

makeJsonGetRequest :: FromJSON a
                   => Text -> Text -> [(Text, Text)]
                   -> IO (Either BinanceError a)
makeJsonGetRequest baseUrl endpoint params =
    fmap (fmap responseBody) $ makeGetRequest baseUrl endpoint params jsonResponse

getBookTicker :: Text -> Text -> IO (Either BinanceError BookTicker)
getBookTicker baseUrl symbol =
    makeJsonGetRequest baseUrl bookTickerEndpoint [("symbol", symbol)]

getAccountInfo :: Text -> Text -> Text -> IO (Either BinanceError AccountInfo)
getAccountInfo baseUrl apiKey apiSecret = do
    posixTime <- getPOSIXTime
    let ts       = T.pack $ show (floor (posixTime * 1000) :: Integer)
        queryStr = "timestamp=" <> ts
        sig      = Auth.signQueryString apiSecret queryStr
        params   = [("timestamp", ts), ("signature", sig)]
    makeAuthJsonGetRequest baseUrl accountEndpoint params apiKey

getTradeFees :: Text -> Text -> Text -> IO (Either BinanceError [TradeFee])
getTradeFees baseUrl apiKey apiSecret = do
    posixTime <- getPOSIXTime
    let ts       = T.pack $ show (floor (posixTime * 1000) :: Integer)
        queryStr = "timestamp=" <> ts
        sig      = Auth.signQueryString apiSecret queryStr
        params   = [("timestamp", ts), ("signature", sig)]
    makeAuthJsonGetRequest baseUrl tradeFeeEndpoint params apiKey

placeMarketOrder
    :: Text
    -> Text
    -> Text
    -> Text
    -> Text
    -> MarketOrderQty
    -> IO (Either BinanceError OrderResponse)
placeMarketOrder baseUrl apiKey apiSecret symbol side marketQty = do
    posixTime <- getPOSIXTime
    let ts           = T.pack $ show (floor (posixTime * 1000) :: Integer)
        (qtyKey, qtyVal) = case marketQty of
            QtyBase  q -> ("quantity",      T.pack $ show q)
            QtyQuote q -> ("quoteOrderQty", T.pack $ show q)
        queryStr = "symbol="   <> symbol
                <> "&side="    <> side
                <> "&type=MARKET"
                <> "&" <> qtyKey <> "=" <> qtyVal
                <> "&timestamp=" <> ts
        sig    = Auth.signQueryString apiSecret queryStr
        params = [ ("symbol",    symbol)
                 , ("side",      side)
                 , ("type",      "MARKET")
                 , (qtyKey,      qtyVal)
                 , ("timestamp", ts)
                 , ("signature", sig)
                 ]
    makeAuthJsonPostRequest baseUrl orderEndpoint params apiKey

makeAuthJsonPostRequest :: FromJSON a
                        => Text -> Text -> [(Text, Text)] -> Text
                        -> IO (Either BinanceError a)
makeAuthJsonPostRequest baseUrl endpoint params apiKey =
    fmap responseBody <$> makeAuthPostRequest baseUrl endpoint params apiKey jsonResponse

makeAuthPostRequest :: HttpResponse response
                    => Text -> Text -> [(Text, Text)] -> Text -> Proxy response
                    -> IO (Either BinanceError response)
makeAuthPostRequest baseUrl endpoint params apiKey responseType = do
    result <- try $ runReq defaultHttpConfig $ do
        let (url, reqParams) = mkRequest baseUrl endpoint params
            authHeader       = header "X-MBX-APIKEY" (TE.encodeUtf8 apiKey)
        req POST url NoReqBody responseType (reqParams <> authHeader)
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (T.pack $ show e)
        Right resp                -> return $ Right resp

makeAuthJsonGetRequest :: FromJSON a
                       => Text -> Text -> [(Text, Text)] -> Text
                       -> IO (Either BinanceError a)
makeAuthJsonGetRequest baseUrl endpoint params apiKey =
    fmap (fmap responseBody) $ makeAuthGetRequest baseUrl endpoint params apiKey jsonResponse

makeAuthGetRequest :: HttpResponse response
                   => Text -> Text -> [(Text, Text)] -> Text -> Proxy response
                   -> IO (Either BinanceError response)
makeAuthGetRequest baseUrl endpoint params apiKey responseType = do
    result <- try $ runReq defaultHttpConfig $ do
        let (url, reqParams) = mkRequest baseUrl endpoint params
            authHeader       = header "X-MBX-APIKEY" (TE.encodeUtf8 apiKey)
        response <- req GET url NoReqBody responseType (reqParams <> authHeader)
        return response
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (T.pack $ show e)
        Right resp                -> return $ Right resp

makeGetRequest :: HttpResponse response
               => Text -> Text -> [(Text, Text)] -> Proxy response -> IO (Either BinanceError response)
makeGetRequest baseUrl endpoint params responseType = do
    result <- try $ runReq defaultHttpConfig $ do
        let (url, reqParams) = mkRequest baseUrl endpoint params
        response <- req GET url NoReqBody responseType reqParams
        return response
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (T.pack $ show e)
        Right resp                -> return $ Right resp

mkRequest :: Text -> Text -> [(Text, Text)] -> (Url 'Https, Option 'Https)
mkRequest baseUrl endpoint params =
    let host      = T.drop (T.length "https://") baseUrl
        pathParts = filter (not . T.null) $ T.splitOn "/" endpoint
        url       = foldl (/:) (https host) pathParts
        reqParams = mconcat $ map (\(k, v) -> k =: v) params
    in (url, reqParams)
