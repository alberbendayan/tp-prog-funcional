{-# LANGUAGE OverloadedStrings #-}

module Binance.API.Conversion
    ( tradeFeeMap
    , buildMarketSnapshotWithFees
    , TickerResult(..)
    , fetchBookTickersForPairs
    , generateAllPairs
    , orderResponseToFill
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
import Data.Time.Clock (UTCTime)

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

tradeFeeMap :: [TradeFee] -> Map Pair CommissionRate
tradeFeeMap fees = Map.fromList $ mapMaybe toEntry fees
  where
    toEntry (TradeFee sym (FeeRate rate)) = do
        pair <- symbolToPair sym
        return (pair, CommissionRate rate)

tickersToQuoteMapWithFees
    :: [BookTicker] -> Map Pair CommissionRate -> CommissionRate -> Map Pair PairQuote
tickersToQuoteMapWithFees bookTickers feeMap defaultCommission =
    Map.fromList $ mapMaybe convertWithFee bookTickers
  where
    convertWithFee bt = do
        pair <- symbolToPair (btSymbol bt)
        let commission = Map.findWithDefault defaultCommission pair feeMap
        return (pair, bookTickerToPairQuote commission bt)

buildMarketSnapshotWithFees
    :: [BookTicker] -> Map Pair CommissionRate -> CommissionRate -> MarketSnapshot
buildMarketSnapshotWithFees bookTickers feeMap defaultCommission = MarketSnapshot
    { snapshotQuotes = tickersToQuoteMapWithFees bookTickers feeMap defaultCommission
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

orderResponseToFill :: OrderResponse -> OrderSide -> Pair -> UTCTime -> Either String Fill
orderResponseToFill resp side pair time
    | null (orFills resp) = Left "orderResponseToFill: no fills en la respuesta"
    | otherwise =
        let fills      = orFills resp
            totalQty   = sum $ map ofQty fills
            weightedP  = sum $ map (\f -> ofQty f * unPrice (ofPrice f)) fills
            avgPrice   = if totalQty > 0 then weightedP / totalQty else 0
            totalFee   = sum $ map ofCommission fills
            feeAsset   = ofCommissionAsset (head fills)
        in Right $ Fill
            { fillPair       = pair
            , fillSide       = side
            , fillAmountBase = orExecutedQty resp
            , fillPrice      = Price avgPrice
            , fillFee        = totalFee
            , fillFeeAsset   = feeAsset
            , fillTime       = time
            }
