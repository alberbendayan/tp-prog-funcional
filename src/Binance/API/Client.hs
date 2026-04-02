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
import qualified Data.ByteString.Char8 as BSC
import Data.Proxy (Proxy)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Req

data BinanceError 
    = NetworkError String
    | ParseError String
    | APIError Int String
    deriving (Show, Eq)

instance Exception BinanceError

ping :: String -> IO (Either BinanceError Bool)
ping baseUrl = do
    result <- makeGetRequest baseUrl pingEndpoint [] ignoreResponse
    case result of
        Left err -> return $ Left err
        Right _ -> return $ Right True

makeJsonGetRequest :: FromJSON a 
                   => String -> Text -> [(Text, Text)] 
                   -> IO (Either BinanceError a)
makeJsonGetRequest baseUrl endpoint params = 
    fmap (fmap responseBody) $ makeGetRequest baseUrl endpoint params jsonResponse

getBookTicker :: String -> Text -> IO (Either BinanceError BookTicker)
getBookTicker baseUrl symbol =
    makeJsonGetRequest baseUrl bookTickerEndpoint [("symbol", symbol)]

getAccountInfo :: String -> String -> String -> IO (Either BinanceError AccountInfo)
getAccountInfo baseUrl apiKey apiSecret = do
    posixTime <- getPOSIXTime
    let ts       = show (floor (posixTime * 1000) :: Integer)
        queryStr = "timestamp=" ++ ts
        sig      = Auth.signQueryString apiSecret queryStr
        params   = [("timestamp", T.pack ts), ("signature", T.pack sig)]
    makeAuthJsonGetRequest baseUrl accountEndpoint params apiKey

getTradeFees :: String -> String -> String -> IO (Either BinanceError [TradeFee])
getTradeFees baseUrl apiKey apiSecret = do
    posixTime <- getPOSIXTime
    let ts       = show (floor (posixTime * 1000) :: Integer)
        queryStr = "timestamp=" ++ ts
        sig      = Auth.signQueryString apiSecret queryStr
        params   = [("timestamp", T.pack ts), ("signature", T.pack sig)]
    makeAuthJsonGetRequest baseUrl tradeFeeEndpoint params apiKey

-- | Coloca una orden de mercado en Binance.
-- La cantidad se especifica con MarketOrderQty:
--   QtyBase  q → envía `quantity=q`       (típico para SELL del base).
--   QtyQuote q → envía `quoteOrderQty=q`  (típico para BUY: cuánto quote gastamos).
placeMarketOrder
    :: String          -- baseUrl
    -> String          -- apiKey
    -> String          -- apiSecret
    -> Text            -- symbol (ej. "BTCUSDT")
    -> Text            -- side: "BUY" | "SELL"
    -> MarketOrderQty  -- cantidad y cómo especificarla
    -> IO (Either BinanceError OrderResponse)
placeMarketOrder baseUrl apiKey apiSecret symbol side marketQty = do
    posixTime <- getPOSIXTime
    let ts           = show (floor (posixTime * 1000) :: Integer)
        (qtyKey, qtyVal) = case marketQty of
            QtyBase  q -> ("quantity",       show q)
            QtyQuote q -> ("quoteOrderQty",  show q)
        queryStr = "symbol="   ++ T.unpack symbol
                ++ "&side="    ++ T.unpack side
                ++ "&type=MARKET"
                ++ "&" ++ qtyKey ++ "=" ++ qtyVal
                ++ "&timestamp=" ++ ts
        sig    = Auth.signQueryString apiSecret queryStr
        params = [ ("symbol",         symbol)
                 , ("side",           side)
                 , ("type",           "MARKET")
                 , (T.pack qtyKey,    T.pack qtyVal)
                 , ("timestamp",      T.pack ts)
                 , ("signature",      T.pack sig)
                 ]
    makeAuthJsonPostRequest baseUrl orderEndpoint params apiKey

makeAuthJsonPostRequest :: FromJSON a
                        => String -> Text -> [(Text, Text)] -> String
                        -> IO (Either BinanceError a)
makeAuthJsonPostRequest baseUrl endpoint params apiKey =
    fmap responseBody <$> makeAuthPostRequest baseUrl endpoint params apiKey jsonResponse

makeAuthPostRequest :: HttpResponse response
                    => String -> Text -> [(Text, Text)] -> String -> Proxy response
                    -> IO (Either BinanceError response)
makeAuthPostRequest baseUrl endpoint params apiKey responseType = do
    result <- try $ runReq defaultHttpConfig $ do
        let (url, reqParams) = mkRequest baseUrl endpoint params
            authHeader       = header "X-MBX-APIKEY" (BSC.pack apiKey)
        req POST url NoReqBody responseType (reqParams <> authHeader)
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
        Right resp                -> return $ Right resp

makeAuthJsonGetRequest :: FromJSON a
                       => String -> Text -> [(Text, Text)] -> String
                       -> IO (Either BinanceError a)
makeAuthJsonGetRequest baseUrl endpoint params apiKey =
    fmap (fmap responseBody) $ makeAuthGetRequest baseUrl endpoint params apiKey jsonResponse

makeAuthGetRequest :: HttpResponse response
                   => String -> Text -> [(Text, Text)] -> String -> Proxy response
                   -> IO (Either BinanceError response)
makeAuthGetRequest baseUrl endpoint params apiKey responseType = do
    result <- try $ runReq defaultHttpConfig $ do
        let (url, reqParams) = mkRequest baseUrl endpoint params
            authHeader       = header "X-MBX-APIKEY" (BSC.pack apiKey)
        response <- req GET url NoReqBody responseType (reqParams <> authHeader)
        return response
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
        Right resp                -> return $ Right resp

makeGetRequest :: HttpResponse response 
               => String -> Text -> [(Text, Text)] -> Proxy response -> IO (Either BinanceError response)
makeGetRequest baseUrl endpoint params responseType = do
    result <- try $ runReq defaultHttpConfig $ do
        let (url, reqParams) = mkRequest baseUrl endpoint params
        response <- req GET url NoReqBody responseType reqParams
        return response
    
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (show e)

        Right resp -> return $ Right resp

mkRequest :: String -> Text -> [(Text, Text)] -> (Url 'Https, Option 'Https)
mkRequest baseUrl endpoint params =
    let httpsPrefix = "https://" :: String
        host = T.pack $ drop (length httpsPrefix) baseUrl
        pathParts = filter (not . T.null) $ T.splitOn "/" endpoint
        url = foldl (/:) (https host) pathParts
        reqParams = mconcat $ map (\(k, v) -> k =: v) params
    in (url, reqParams)
