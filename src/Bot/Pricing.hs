{-# LANGUAGE OverloadedStrings #-}

module Bot.Pricing
  ( assetUsdtRateWhenSelling
  , assetUsdtRateWhenBuying
  ) where

import Bot.Domain (Asset(..), MarketSnapshot(..), Pair(..), PairQuote(..), Price(..), UsdtRate(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

assetUsdtRateWhenSelling :: MarketSnapshot -> Asset -> Either T.Text UsdtRate
assetUsdtRateWhenSelling _ USDT = Right (UsdtRate 1)
assetUsdtRateWhenSelling snapshot asset =
  assetUsdtPerUnit (snapshotQuotes snapshot) asset bidPrice askPrice "ask"

assetUsdtRateWhenBuying :: MarketSnapshot -> Asset -> Either T.Text UsdtRate
assetUsdtRateWhenBuying _ USDT = Right (UsdtRate 1)
assetUsdtRateWhenBuying snapshot asset =
  assetUsdtPerUnit (snapshotQuotes snapshot) asset askPrice bidPrice "bid"

assetUsdtPerUnit
  :: Map Pair PairQuote
  -> Asset
  -> (PairQuote -> Price)
  -> (PairQuote -> Price)
  -> T.Text
  -> Either T.Text UsdtRate
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

    directErr  = "Sin cotización directa a USDT para " <> T.pack (show asset)
    inverseErr = "Sin cotización inversa a USDT para " <> T.pack (show asset)

requirePairQuote :: Map Pair PairQuote -> Pair -> T.Text -> Either T.Text PairQuote
requirePairQuote quotes pair errMsg =
  maybe (Left errMsg) Right (M.lookup pair quotes)

usdtPerAssetDirect :: PairQuote -> (PairQuote -> Price) -> Pair -> Either T.Text UsdtRate
usdtPerAssetDirect q pick pair =
  let px = unPrice (pick q)
   in if px <= 0
        then Left $ "Cotización inválida para " <> T.pack (show pair)
        else Right (UsdtRate px)

usdtPerAssetInverse :: PairQuote -> (PairQuote -> Price) -> T.Text -> Pair -> Either T.Text UsdtRate
usdtPerAssetInverse q pick label pair =
  let px = unPrice (pick q)
   in if px <= 0
        then Left $ "Cotización inválida (" <> label <> ") para " <> T.pack (show pair)
        else Right (UsdtRate (1 / px))

orElse :: Either T.Text a -> Either T.Text a -> Either T.Text a
orElse (Right x) _ = Right x
orElse (Left _) y  = y
