{-# LANGUAGE RecordWildCards #-}

module FakeExchange.Instance
  ( FakeExchange(..)
  , FakeExchangeState(..)
  ) where

import Exchange.Interface
import Bot.Domain
  ( Asset(..)
  , CommissionRate(..)
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
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, getCurrentTime, utctDayTime)

data FakeExchangeState = FakeExchangeState
  { fesQuotes   :: Map Pair PairQuote
  , fesBalances :: Map Asset Double
  }

newtype FakeExchange = FakeExchange
  { fakeExchangeRef :: IORef FakeExchangeState
  }

instance Exchange FakeExchange where
  checkConnectivity _ = return (Right True)

  fetchMarketSnapshot (FakeExchange ref) assets = liftIO $ do
    now <- getCurrentTime
    FakeExchangeState{..} <- atomicModifyIORef' ref $ \st ->
      let movedQuotes = evolveQuotes now (fesQuotes st)
      in (st { fesQuotes = movedQuotes }, st { fesQuotes = movedQuotes })
    let pairs = generateAllPairs assets
    return $ marketSnapshotFromBook fesQuotes pairs

  fetchBalances (FakeExchange ref) = liftIO $ do
    FakeExchangeState{..} <- readIORef ref
    return $ Right fesBalances

  executeOrder (FakeExchange ref) step = liftIO $ do
    now <- getCurrentTime
    atomicModifyIORef' ref (runOrderAgainstState now step)

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

runOrderAgainstState
  :: UTCTime
  -> OrderStep
  -> FakeExchangeState
  -> (FakeExchangeState, Either ExchangeError Fill)
runOrderAgainstState now step st =
  either keepState applySuccess (buildOrderExecution now step st)
  where
    keepState err = (st, Left err)
    applySuccess (fill, nextBalances) =
      (st { fesBalances = nextBalances }, Right fill)

buildOrderExecution
  :: UTCTime
  -> OrderStep
  -> FakeExchangeState
  -> Either ExchangeError (Fill, Map Asset Double)
buildOrderExecution now step st = do
  quote <- quoteForOrder (fesQuotes st) (stepPair step)
  fill <- asExchangeOrderError (syntheticFill step quote now)
  nextBalances <- asExchangeOrderError (applyFillToBalances (fesBalances st) fill)
  Right (fill, nextBalances)

asExchangeOrderError :: Either String a -> Either ExchangeError a
asExchangeOrderError = either (Left . ExchangeOrderError) Right

quoteForOrder :: Map Pair PairQuote -> Pair -> Either ExchangeError PairQuote
quoteForOrder book p =
  maybe (Left $ unknownPair p) Right (Map.lookup p book)
  where
    unknownPair :: Pair -> ExchangeError
    unknownPair x =
      ExchangeOrderError $ "FakeExchange: par desconocido " ++ show x

-- | Coherente con las fórmulas de 'Bot.Arbitraje.simulateOneStep'.
syntheticFill :: OrderStep -> PairQuote -> UTCTime -> Either String Fill
syntheticFill step pq now = mkSyntheticFill step pq now

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

applyFillToBalances :: Map Asset Double -> Fill -> Either String (Map Asset Double)
applyFillToBalances balances fill =
  case fillSide fill of
    Sell -> applySellFill balances fill
    Buy  -> applyBuyFill balances fill
  where
    pair = fillPair fill

    applySellFill bs f = do
      withBaseDebited <- debitAsset (base pair) (fillAmountBase f) bs
      Right $ creditAsset (quote pair) (sellNetQuote f) withBaseDebited

    applyBuyFill bs f = do
      withQuoteDebited <- debitAsset (quote pair) (buyQuoteSpent f) bs
      withFeeDebited <- debitAsset (fillFeeAsset f) (fillFee f) withQuoteDebited
      Right $ creditAsset (base pair) (fillAmountBase f) withFeeDebited

    sellNetQuote f = fillAmountBase f * unPrice (fillPrice f) - fillFee f
    buyQuoteSpent f = fillAmountBase f * unPrice (fillPrice f)

debitAsset :: Asset -> Double -> Map Asset Double -> Either String (Map Asset Double)
debitAsset asset amount balances
  | amount <= 0 = Right balances
  | available + epsilon < amount =
      Left $
        "FakeExchange: balance insuficiente en "
          ++ show asset
          ++ " (disponible="
          ++ show available
          ++ ", requerido="
          ++ show amount
          ++ ")"
  | otherwise =
      Right $ Map.insert asset (available - amount) balances
  where
    available = Map.findWithDefault 0 asset balances
    epsilon = 1e-12

creditAsset :: Asset -> Double -> Map Asset Double -> Map Asset Double
creditAsset asset amount balances
  | amount <= 0 = balances
  | otherwise = Map.insertWith (+) asset amount balances

evolveQuotes :: UTCTime -> Map Pair PairQuote -> Map Pair PairQuote
evolveQuotes now = Map.mapWithKey (applyPairMove now)

applyPairMove :: UTCTime -> Pair -> PairQuote -> PairQuote
applyPairMove now pair pq =
  let t = realToFrac (utctDayTime now) :: Double
      pairSeed = fromIntegral (assetIndex (base pair) * 13 + assetIndex (quote pair) * 7)
      wave = sin (t / 11 + pairSeed) + 0.5 * cos (t / 17 + pairSeed / 3)
      movePct = 0.0009 * wave
      mid = (unPrice (bidPrice pq) + unPrice (askPrice pq)) / 2
      spread = max 1e-12 (unPrice (askPrice pq) - unPrice (bidPrice pq))
      movedMid = max 1e-12 (mid * (1 + movePct))
      nextBid = max 1e-12 (movedMid - spread / 2)
      nextAsk = max (nextBid + 1e-12) (movedMid + spread / 2)
  in pq { bidPrice = Price nextBid, askPrice = Price nextAsk }

assetIndex :: Asset -> Int
assetIndex BTC = 1
assetIndex ETH = 2
assetIndex USDT = 3
assetIndex BNB = 4
