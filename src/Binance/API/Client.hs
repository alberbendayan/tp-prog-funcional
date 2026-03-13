{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Binance.API.Client
    ( ping
    , getBookTicker
    , BinanceError(..)
    ) where

import Binance.API.Types
import Binance.API.Endpoints
import Control.Exception (Exception, try, SomeException)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy (Proxy)
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
