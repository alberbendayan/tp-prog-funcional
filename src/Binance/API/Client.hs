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
-- try :: Exception e => IO a -> IO (Either e a)
import Data.Aeson (FromJSON)
-- FromJSON :: Type class for types that can be parsed from JSON
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

-- Función auxiliar que hace GET request JSON y extrae el responseBody
-- fmap (fmap responseBody) aplica dos transformaciones:
--   1. El primer fmap mapea sobre IO (el contexto externo)
--   2. El segundo fmap mapea sobre Either (el contexto interno)
-- Resultado: IO (Either BinanceError response) → IO (Either BinanceError a)
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
