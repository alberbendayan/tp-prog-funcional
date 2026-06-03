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
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad (guard)
import Data.Time.Clock (UTCTime)

parseAsset :: Text -> Maybe Asset
parseAsset "BTC"  = Just BTC
parseAsset "ETH"  = Just ETH
parseAsset "USDT" = Just USDT
parseAsset "BNB"  = Just BNB
parseAsset _      = Nothing

knownAssets :: [(Text, Asset)]
knownAssets = [("BTC", BTC), ("ETH", ETH), ("USDT", USDT), ("BNB", BNB)]

parseSymbol :: Text -> Maybe Pair
parseSymbol symbol = listToMaybe $ mapMaybe tryOne knownAssets
  where
    tryOne (baseStr, baseAsset) = do
        guard (T.isPrefixOf baseStr symbol)
        quoteAsset <- parseAsset (T.drop (T.length baseStr) symbol)
        return (Pair baseAsset quoteAsset)

symbolToPair :: Symbol -> Maybe Pair
symbolToPair (Symbol s) = parseSymbol s

bookTickerToPairQuote :: CommissionRate -> BookTicker -> PairQuote
bookTickerToPairQuote commission bt = PairQuote
    { bidPrice       = btBidPrice bt
    , askPrice       = btAskPrice bt
    , pairCommission = commission
    , bidQty         = btBidQty bt
    , askQty         = btAskQty bt
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

generateAllPairs :: [Asset] -> [Pair]
generateAllPairs assets = [Pair b q | b <- assets, q <- assets, b /= q]

fetchSingleTicker :: T.Text -> Symbol -> IO TickerResult
fetchSingleTicker url sym@(Symbol rawSym) = do
  result <- Client.getBookTicker url rawSym
  case result of
    Right bt -> return (TickerOk bt)
    Left (Client.NetworkError msg)
      | "Invalid symbol" `T.isInfixOf` msg
      -> return (TickerNotSupported sym)
    Left err -> return (TickerFailed err)

fetchBookTickersForPairs :: T.Text -> [Pair] -> IO [TickerResult]
fetchBookTickersForPairs baseUrl = mapM (fetchSingleTicker baseUrl) . map pairToSymbol

totalFillQty :: [OrderFill] -> Double
totalFillQty = sum . map ofQty

fillWeightedValue :: OrderFill -> Double
fillWeightedValue f = ofQty f * unPrice (ofPrice f)

weightedFillPrice :: [OrderFill] -> Double
weightedFillPrice = sum . map fillWeightedValue

avgFillPrice :: [OrderFill] -> Double
avgFillPrice fills
    | totalFillQty fills > 0 = weightedFillPrice fills / totalFillQty fills
    | otherwise              = 0

totalFillFee :: [OrderFill] -> Double
totalFillFee = sum . map ofCommission

buildFill :: [OrderFill] -> OrderResponse -> OrderSide -> Pair -> UTCTime -> Fill
buildFill fills resp side pair time = Fill
    { fillPair       = pair
    , fillSide       = side
    , fillAmountBase = orExecutedQty resp
    , fillPrice      = Price (avgFillPrice fills)
    , fillFee        = totalFillFee fills
    , fillFeeAsset   = ofCommissionAsset (head fills)
    , fillTime       = time
    }

orderResponseToFill :: OrderResponse -> OrderSide -> Pair -> UTCTime -> Either T.Text Fill
orderResponseToFill resp side pair time
    | null (orFills resp) = Left "orderResponseToFill: no fills en la respuesta"
    | otherwise           = Right $ buildFill (orFills resp) resp side pair time
