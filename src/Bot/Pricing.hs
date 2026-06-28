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
      usdtRateFromPrice id invalidDirectErr (directPick q)

    usdtFromInversePair = do
      q <- requirePairQuote quotes inversePair inverseErr
      usdtRateFromPrice (1 /) invalidInverseErr (inversePick q)

    directErr  = "Sin cotización directa a USDT para " <> T.pack (show asset)
    inverseErr = "Sin cotización inversa a USDT para " <> T.pack (show asset)

    invalidDirectErr  = "Cotización inválida para " <> T.pack (show directPair)
    invalidInverseErr = "Cotización inválida (" <> inverseLabel <> ") para " <> T.pack (show inversePair)

requirePairQuote :: Map Pair PairQuote -> Pair -> T.Text -> Either T.Text PairQuote
requirePairQuote quotes pair errMsg =
  maybe (Left errMsg) Right (M.lookup pair quotes)

usdtRateFromPrice :: (Double -> Double) -> T.Text -> Price -> Either T.Text UsdtRate
usdtRateFromPrice transform errMsg (Price px)
  | px <= 0   = Left errMsg
  | otherwise = Right (UsdtRate (transform px))

orElse :: Either T.Text a -> Either T.Text a -> Either T.Text a
orElse (Right x) _ = Right x
orElse (Left _) y  = y
