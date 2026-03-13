{-# LANGUAGE OverloadedStrings #-}

module Binance.API.Conversion
    ( parseSymbol
    , symbolToPair
    , bookTickerToPairQuote
    , buildMarketSnapshot
    , TickerResult(..)
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
import Data.List (isInfixOf)

parseAsset :: Text -> Maybe Asset
parseAsset "BTC"  = Just BTC
parseAsset "ETH"  = Just ETH
parseAsset "USDT" = Just USDT
parseAsset "BNB"  = Just BNB
parseAsset _      = Nothing

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

symbolToPair :: Symbol -> Maybe Pair
symbolToPair (Symbol s) = parseSymbol s

bookTickerToPairQuote :: CommissionRate -> BookTicker -> PairQuote
bookTickerToPairQuote commission bt = PairQuote
    { bidPrice       = btBidPrice bt
    , askPrice       = btAskPrice bt
    , pairCommission = commission
    }

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

data TickerResult
  = TickerOk BookTicker            
  | TickerNotSupported Symbol      
  | TickerFailed Client.BinanceError 



pairsForBase :: [Asset] -> Asset -> [Pair]
pairsForBase assets baseAsset =
    let validQuotes = filter (/= baseAsset) assets
    in  map (Pair baseAsset) validQuotes

generateAllPairs :: [Asset] -> [Pair]
generateAllPairs assets =
    concatMap (pairsForBase assets) assets

fetchSingleTicker :: String -> Symbol -> IO TickerResult
fetchSingleTicker url sym@(Symbol rawSym) = do
  result <- Client.getBookTicker url rawSym
  case result of
    Right bt -> return (TickerOk bt)
    Left (Client.NetworkError msg)
      | "Invalid symbol" `isInfixOf` msg
      -> return (TickerNotSupported sym)
    Left err -> return (TickerFailed err)

fetchBookTickersForPairs :: String -> [Pair] -> IO [TickerResult]
fetchBookTickersForPairs baseUrl pairs = do
    let symbols = map pairToSymbol pairs
    results <- mapM (fetchSingleTicker baseUrl) symbols
    return results
