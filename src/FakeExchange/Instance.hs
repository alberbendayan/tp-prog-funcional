{-# LANGUAGE RecordWildCards #-}

module FakeExchange.Instance
  ( FakeExchange(..)
  , FakeExchangeState(..)
  ) where

import Exchange.Interface
import Bot.Domain
  ( CommissionRate(..)
  , MarketSnapshot(..)
  , OrderSide(..)
  , OrderStep(..)
  , Fill(..)
  , PairQuote(..)
  )
import Binance.API.Types (Pair(..), Price(..), MarketOrderQty(..))
import Binance.API.Conversion (generateAllPairs)
import Data.Bifunctor (first)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)

-- | Estado mutable del libro de órdenes simulado (bid/ask por par).
data FakeExchangeState = FakeExchangeState
  { fesQuotes :: Map Pair PairQuote
  }

-- | Exchange simulado; las lecturas y órdenes usan el mismo 'IORef'.
newtype FakeExchange = FakeExchange
  { fakeExchangeRef :: IORef FakeExchangeState
  }

instance Exchange FakeExchange where
  checkConnectivity _ = return (Right True)

  fetchMarketSnapshot (FakeExchange ref) assets = liftIO $ do
    FakeExchangeState{..} <- readIORef ref
    let required = generateAllPairs assets
        missing = filter (not . (`Map.member` fesQuotes)) required
    case missing of
      (p:_) -> return $ Left $ ExchangeFetchError $
        "FakeExchange: falta cotización para el par " ++ show p
      [] ->
        let snapQuotes = Map.fromList
              [ (p, fesQuotes Map.! p) | p <- required ]
        in return $ Right $ MarketSnapshot { snapshotQuotes = snapQuotes }

  executeOrder (FakeExchange ref) step = liftIO $ do
    FakeExchangeState{..} <- readIORef ref
    let p = stepPair step
    case Map.lookup p fesQuotes of
      Nothing ->
        return $ Left $ ExchangeOrderError $
          "FakeExchange: par desconocido " ++ show p
      Just pq -> do
        ef <- syntheticFill step pq
        return $ first ExchangeOrderError ef

-- | Coherente con las fórmulas de 'Bot.Arbitraje.simulateOneStep'.
syntheticFill :: OrderStep -> PairQuote -> IO (Either String Fill)
syntheticFill step pq = do
  now <- getCurrentTime
  let fee = unCommissionRate (pairCommission pq)
  case (stepSide step, stepQty step) of
    (Sell, QtyBase q) ->
      let bid = unPrice (bidPrice pq)
          feePaid = q * bid * fee
      in return $ Right $ Fill
        { fillPair       = stepPair step
        , fillSide       = Sell
        , fillAmountBase = q
        , fillPrice      = bidPrice pq
        , fillFee        = feePaid
        , fillFeeAsset   = quote (stepPair step)
        , fillTime       = now
        }
    (Buy, QtyQuote q) ->
      let ask = unPrice (askPrice pq)
          feePaid = q * fee
          baseOut = q / ask * (1 - fee)
      in return $ Right $ Fill
        { fillPair       = stepPair step
        , fillSide       = Buy
        , fillAmountBase = baseOut
        , fillPrice      = askPrice pq
        , fillFee        = feePaid
        , fillFeeAsset   = quote (stepPair step)
        , fillTime       = now
        }
    _ ->
      return $ Left
        "FakeExchange: solo se simulan ventas en base o compras en quote (como el plan del bot)"
