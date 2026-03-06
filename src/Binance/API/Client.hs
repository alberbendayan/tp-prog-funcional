{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Binance.API.Client
    ( ping
    , getPrice
    , getAllPrices
    , BinanceError(..)
    ) where

import Binance.API.Types
import Binance.API.Endpoints
import Control.Exception (Exception, try, SomeException)
-- try :: Exception e => IO a -> IO (Either e a)
import Data.Text (Text)
import qualified Data.Text as T
-- T.pack :: String -> Text
-- T.splitOn :: Text -> Text -> [Text]
-- T.null :: Text -> Bool
import Data.Proxy (Proxy)
import Network.HTTP.Req
-- runReq :: MonadHttp m => HttpConfig -> m a -> m a
-- req :: (HttpMethod method, HttpBody body, HttpResponse response, HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) 
--     => method -> Url scheme -> body -> Proxy response -> Option scheme -> Req response
-- https :: Text -> Url 'Https
-- (/:) :: Url scheme -> Text -> Url scheme
-- (=:) :: ToHttpApiData a => Text -> a -> Option scheme
-- ignoreResponse :: HttpResponse IgnoreResponse
-- jsonResponse :: FromJSON a => Proxy (JsonResponse a)
-- responseBody :: HttpResponse response => response -> HttpResponseBody response

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

getPrice :: String -> Text -> IO (Either BinanceError TickerPrice)
getPrice baseUrl symbol = do
    result <- makeGetRequest baseUrl tickerPriceEndpoint [("symbol", symbol)] jsonResponse
    case result of
        Left err -> return $ Left err
        Right jsonResp -> return $ Right $ responseBody jsonResp

getAllPrices :: String -> IO (Either BinanceError [TickerPrice])
getAllPrices baseUrl = do
    result <- makeGetRequest baseUrl tickerPriceEndpoint [] jsonResponse
    case result of
        Left err -> return $ Left err
        Right jsonResp -> return $ Right $ responseBody jsonResp


makeGetRequest :: HttpResponse response 
               => String -> Text -> [(Text, Text)] -> Proxy response -> IO (Either BinanceError response)
makeGetRequest baseUrl endpoint params responseType = do
    result <- try $ runReq defaultHttpConfig $ do
        let (url, reqParams) = mkRequest baseUrl endpoint params
        response <- req GET url NoReqBody responseType reqParams
        return response
    
    case result of
        Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
        -- return :: Monad m => a -> m a
        -- show :: Show a => a -> String
        Right resp -> return $ Right resp

mkRequest :: String -> Text -> [(Text, Text)] -> (Url 'Https, Option 'Https)
mkRequest baseUrl endpoint params =
    let httpsPrefix = "https://" :: String
        host = T.pack $ drop (length httpsPrefix) baseUrl
        -- drop :: Int -> [a] -> [a]
        -- length :: [a] -> Int
        pathParts = filter (not . T.null) $ T.splitOn "/" endpoint
        -- filter :: (a -> Bool) -> [a] -> [a]
        -- not :: Bool -> Bool
        url = foldl (/:) (https host) pathParts
        -- foldl :: (b -> a -> b) -> b -> [a] -> b
        reqParams = mconcat $ map (\(k, v) -> k =: v) params
        -- (=:) :: ToHttpApiData a => Text -> a -> Option scheme
        -- mconcat :: Monoid a => [a] -> a
        -- map :: (a -> b) -> [a] -> [b]
    in (url, reqParams)
