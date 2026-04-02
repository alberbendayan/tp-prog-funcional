{-# LANGUAGE RecordWildCards #-}

module FakeExchange.Control
  ( newFakeExchange
  , readFakeQuotes
  , writeFakeQuotes
  , modifyFakeQuotes
  , setPairQuote
  , defaultDemoQuotes
  , newDemoFakeExchange
  ) where

import FakeExchange.Instance
import Bot.Domain (PairQuote(..), CommissionRate(..))
import Binance.API.Types (Asset(..), Pair(..), Price(..))
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

demoFee :: CommissionRate
demoFee = CommissionRate 0.001

-- | Bid/ask con la comisión fija de la demo ('demoFee').
mkDemoPairQuote :: Double -> Double -> PairQuote
mkDemoPairQuote bid ask = PairQuote (Price bid) (Price ask) demoFee


defaultDemoQuotes :: Map Pair PairQuote
defaultDemoQuotes =
  let goodUSDTBTC = mkDemoPairQuote 0.0000208 0.0000212
      goodBTCETH  = mkDemoPairQuote 19.5      20.5
      goodETHUSDT = mkDemoPairQuote 2550      2650
      overrides = Map.fromList
        [ (Pair USDT BTC, goodUSDTBTC)
        , (Pair BTC ETH,  goodBTCETH)
        , (Pair ETH USDT, goodETHUSDT)
        ]
  in Map.union overrides validSupportingPairs


validSupportingPairs :: Map Pair PairQuote
validSupportingPairs = Map.fromList
  [ (Pair BTC USDT, mkDemoPairQuote 49900 50100)
  , (Pair BTC ETH,  mkDemoPairQuote 19.8 20.2)
  , (Pair BTC BNB,  mkDemoPairQuote 0.0098 0.0102)
  , (Pair ETH BTC,  mkDemoPairQuote 0.049 0.051)
  , (Pair ETH BNB,  mkDemoPairQuote 4.95 5.05)
  , (Pair ETH USDT, mkDemoPairQuote 2490 2510)
  , (Pair BNB BTC,  mkDemoPairQuote 0.0098 0.0102)
  , (Pair BNB ETH,  mkDemoPairQuote 0.198 0.202)
  , (Pair BNB USDT, mkDemoPairQuote 498 502)
  , (Pair USDT BTC, mkDemoPairQuote 1.96e-5 2.04e-5)
  , (Pair USDT ETH, mkDemoPairQuote 0.000396 0.000404)
  , (Pair USDT BNB, mkDemoPairQuote 0.00198 0.00202)
  ]

newFakeExchange :: Map Pair PairQuote -> IO FakeExchange
newFakeExchange initial =
  FakeExchange <$> newIORef (FakeExchangeState initial)

newDemoFakeExchange :: IO FakeExchange
newDemoFakeExchange = newFakeExchange defaultDemoQuotes

readFakeQuotes :: FakeExchange -> IO (Map Pair PairQuote)
readFakeQuotes (FakeExchange ref) = fmap fesQuotes $ readIORef ref

writeFakeQuotes :: FakeExchange -> Map Pair PairQuote -> IO ()
writeFakeQuotes (FakeExchange ref) qs =
  writeIORef ref FakeExchangeState { fesQuotes = qs }

modifyFakeQuotes :: FakeExchange -> (Map Pair PairQuote -> Map Pair PairQuote) -> IO ()
modifyFakeQuotes (FakeExchange ref) f =
  modifyIORef' ref $ \s -> s { fesQuotes = f (fesQuotes s) }

setPairQuote :: FakeExchange -> Pair -> PairQuote -> IO ()
setPairQuote fe pair quote = modifyFakeQuotes fe (Map.insert pair quote)
