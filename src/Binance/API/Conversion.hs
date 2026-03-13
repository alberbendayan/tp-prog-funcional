{-# LANGUAGE OverloadedStrings #-}

module Binance.API.Conversion
    ( parseSymbol
    , symbolToPair
    , bookTickerToPairQuote
    , buildMarketSnapshot
    , fetchBookTickersForPairs
    , generateAllPairs
    ) where

import Binance.API.Types
import qualified Binance.API.Client as Client
import Bot.Domain
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

-- | Parsea un símbolo de texto a un Asset
parseAsset :: Text -> Maybe Asset
parseAsset "BTC"  = Just BTC
parseAsset "ETH"  = Just ETH
parseAsset "USDT" = Just USDT
parseAsset "BNB"  = Just BNB
parseAsset _      = Nothing

-- | Parsea un símbolo como "BTCUSDT" a un Pair
-- Intenta todas las combinaciones posibles de assets conocidos
parseSymbol :: Text -> Maybe Pair
parseSymbol symbol = tryParse [("BTC", BTC), ("ETH", ETH), ("USDT", USDT), ("BNB", BNB)]
  where
    tryParse [] = Nothing
    tryParse ((baseStr, baseAsset):rest) =
      if T.isPrefixOf baseStr symbol
        then
          let quoteStr = T.drop (T.length baseStr) symbol
          in case parseAsset quoteStr of
               Just quoteAsset -> Just $ Pair baseAsset quoteAsset
               Nothing -> tryParse rest
        else tryParse rest

-- | Convierte un Symbol a un Pair (usando parseSymbol)
symbolToPair :: Symbol -> Maybe Pair
symbolToPair (Symbol s) = parseSymbol s

-- | Convierte un BookTicker a un PairQuote con su comisión
bookTickerToPairQuote :: CommissionRate -> BookTicker -> PairQuote
bookTickerToPairQuote commission bt = PairQuote
    { bidPrice       = btBidPrice bt
    , askPrice       = btAskPrice bt
    , pairCommission = commission
    }

-- | Construye un MarketSnapshot a partir de una lista de BookTickers
convertTicker :: CommissionRate -> BookTicker -> Maybe (Pair, PairQuote)
convertTicker commission bt = do
    pair <- symbolToPair (btSymbol bt)
    return (pair, bookTickerToPairQuote commission bt)

tickersToQuoteMap :: CommissionRate -> [BookTicker] -> Map Pair PairQuote
tickersToQuoteMap commission bookTickers = Map.fromList (mapMaybe (convertTicker commission) bookTickers)

buildMarketSnapshot :: [BookTicker] -> CommissionRate -> MarketSnapshot
buildMarketSnapshot bookTickers commission = MarketSnapshot
    { snapshotQuotes = tickersToQuoteMap commission bookTickers
    }

pairsForBase :: [Asset] -> Asset -> [Pair]
pairsForBase assets baseAsset =
    let validQuotes = filter (/= baseAsset) assets
    in  map (Pair baseAsset) validQuotes

generateAllPairs :: [Asset] -> [Pair]
generateAllPairs assets =
    concatMap (pairsForBase assets) assets

toMaybe :: Either a b -> Maybe b
toMaybe (Right x) = Just x
toMaybe (Left _)  = Nothing

collectSuccessful :: [Either Client.BinanceError BookTicker] -> [BookTicker]
collectSuccessful = mapMaybe toMaybe

fetchSingleTicker :: String -> Symbol -> IO (Either Client.BinanceError BookTicker)
fetchSingleTicker url (Symbol sym) = Client.getBookTicker url sym

fetchBookTickersForPairs :: String -> [Pair] -> IO [BookTicker]
fetchBookTickersForPairs baseUrl pairs = do
    let symbols = map pairToSymbol pairs
    results <- mapM (fetchSingleTicker baseUrl) symbols
    return $ collectSuccessful results
