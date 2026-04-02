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
  , Pair(..)
  , Price(..)
  , MarketOrderQty(..)
  )
import Binance.API.Conversion (generateAllPairs)
import Data.Bifunctor (first)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, getCurrentTime)

data FakeExchangeState = FakeExchangeState
  { fesQuotes :: Map Pair PairQuote
  }

newtype FakeExchange = FakeExchange
  { fakeExchangeRef :: IORef FakeExchangeState
  }

instance Exchange FakeExchange where
  checkConnectivity _ = return (Right True)

  fetchMarketSnapshot (FakeExchange ref) assets = liftIO $ do
    FakeExchangeState{..} <- readIORef ref
    let pairs = generateAllPairs assets
    return $ marketSnapshotFromBook fesQuotes pairs

  executeOrder (FakeExchange ref) step = liftIO $ do
    FakeExchangeState{..} <- readIORef ref
    case quoteForOrder fesQuotes (stepPair step) of
      Left err    -> return (Left err)
      Right quote -> first ExchangeOrderError <$> syntheticFill step quote

-- | Libro completo para los pares pedidos, o primer par faltante.
marketSnapshotFromBook :: Map Pair PairQuote -> [Pair] -> Either ExchangeError MarketSnapshot
marketSnapshotFromBook book pairs =
  MarketSnapshot . Map.fromList <$> traverse pairEntry pairs
  where
    pairEntry :: Pair -> Either ExchangeError (Pair, PairQuote)
    pairEntry p =
      maybe (Left $ missingFetch p) (\q -> Right (p, q)) (Map.lookup p book)

    missingFetch :: Pair -> ExchangeError
    missingFetch p =
      ExchangeFetchError $
        "FakeExchange: falta cotización para el par " ++ show p

quoteForOrder :: Map Pair PairQuote -> Pair -> Either ExchangeError PairQuote
quoteForOrder book p =
  maybe (Left $ unknownPair p) Right (Map.lookup p book)
  where
    unknownPair :: Pair -> ExchangeError
    unknownPair x =
      ExchangeOrderError $ "FakeExchange: par desconocido " ++ show x

-- | Coherente con las fórmulas de 'Bot.Arbitraje.simulateOneStep'.
syntheticFill :: OrderStep -> PairQuote -> IO (Either String Fill)
syntheticFill step pq = do
  now <- getCurrentTime
  return $ mkSyntheticFill step pq now

mkSyntheticFill :: OrderStep -> PairQuote -> UTCTime -> Either String Fill
mkSyntheticFill step pq now =
  case (stepSide step, stepQty step) of
    (Sell, QtyBase qty)  -> Right $ fillSellBase step pq now qty
    (Buy, QtyQuote qty)  -> Right $ fillBuyQuote step pq now qty
    _ ->
      Left
        "FakeExchange: solo se simulan ventas en base o compras en quote (como el plan del bot)"

fillSellBase :: OrderStep -> PairQuote -> UTCTime -> Double -> Fill
fillSellBase step pq now qty =
  let fee     = unCommissionRate (pairCommission pq)
      bid     = unPrice (bidPrice pq)
      feePaid = qty * bid * fee
  in Fill
        { fillPair       = stepPair step
        , fillSide       = Sell
        , fillAmountBase = qty
        , fillPrice      = bidPrice pq
        , fillFee        = feePaid
        , fillFeeAsset   = quote (stepPair step)
        , fillTime       = now
        }

fillBuyQuote :: OrderStep -> PairQuote -> UTCTime -> Double -> Fill
fillBuyQuote step pq now qtyQuote =
  let fee     = unCommissionRate (pairCommission pq)
      ask     = unPrice (askPrice pq)
      feePaid = qtyQuote * fee
      baseOut = qtyQuote / ask * (1 - fee)
  in Fill
        { fillPair       = stepPair step
        , fillSide       = Buy
        , fillAmountBase = baseOut
        , fillPrice      = askPrice pq
        , fillFee        = feePaid
        , fillFeeAsset   = quote (stepPair step)
        , fillTime       = now
        }
