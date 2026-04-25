module Bot.Pricing
  ( assetUsdtRateWhenSelling
  , assetUsdtRateWhenBuying
  ) where

import Bot.Domain (Asset(..), MarketSnapshot(..), Pair(..), PairQuote(..), Price(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

assetUsdtRateWhenSelling :: MarketSnapshot -> Asset -> Either String Double
assetUsdtRateWhenSelling _ USDT = Right 1
assetUsdtRateWhenSelling snapshot asset =
  assetUsdtPerUnit (snapshotQuotes snapshot) asset bidPrice askPrice "ask"

assetUsdtRateWhenBuying :: MarketSnapshot -> Asset -> Either String Double
assetUsdtRateWhenBuying _ USDT = Right 1
assetUsdtRateWhenBuying snapshot asset =
  assetUsdtPerUnit (snapshotQuotes snapshot) asset askPrice bidPrice "bid"

assetUsdtPerUnit
  :: Map Pair PairQuote
  -> Asset
  -> (PairQuote -> Price)
  -> (PairQuote -> Price)
  -> String
  -> Either String Double
assetUsdtPerUnit quotes asset directPick inversePick inverseLabel =
  usdtFromDirectPair `orElse` usdtFromInversePair
  where
    directPair = Pair asset USDT
    inversePair = Pair USDT asset

    usdtFromDirectPair = do
      q <- requirePairQuote quotes directPair directErr
      usdtPerAssetDirect q directPick directPair

    usdtFromInversePair = do
      q <- requirePairQuote quotes inversePair inverseErr
      usdtPerAssetInverse q inversePick inverseLabel inversePair

    directErr = "Sin cotización directa a USDT para " ++ show asset
    inverseErr = "Sin cotización inversa a USDT para " ++ show asset

requirePairQuote :: Map Pair PairQuote -> Pair -> String -> Either String PairQuote
requirePairQuote quotes pair errMsg =
  maybe (Left errMsg) Right (M.lookup pair quotes)

usdtPerAssetDirect :: PairQuote -> (PairQuote -> Price) -> Pair -> Either String Double
usdtPerAssetDirect q pick pair =
  let px = unPrice (pick q)
   in if px <= 0
        then Left $ "Cotización inválida para " ++ show pair
        else Right px

usdtPerAssetInverse :: PairQuote -> (PairQuote -> Price) -> String -> Pair -> Either String Double
usdtPerAssetInverse q pick label pair =
  let px = unPrice (pick q)
   in if px <= 0
        then Left $ "Cotización inválida (" ++ label ++ ") para " ++ show pair
        else Right (1 / px)

orElse :: Either String a -> Either String a -> Either String a
orElse (Right x) _ = Right x
orElse (Left _) y  = y
