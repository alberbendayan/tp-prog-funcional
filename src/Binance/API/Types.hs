{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Binance.API.Types
    (
      Asset(..)
    , Pair(..)
    , Price(..)
    , MarketOrderQty(..)
    , Symbol(..)
    , FeeRate(..)
    , BookTicker(..)
    , TradeFee(..)
    , AccountInfo(..)
    , OrderFill(..)
    , OrderResponse(..)
    , pairToSymbol
    ) where

import Bot.Domain (Asset(..), Pair(..), Price(..), MarketOrderQty(..))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics

newtype Symbol = Symbol { unSymbol :: Text }
  deriving (Show, Eq, Ord, Generic)

assetToText :: Asset -> Text
assetToText = T.pack . show

pairToSymbol :: Pair -> Symbol
pairToSymbol (Pair b q) = Symbol $ assetToText b <> assetToText q

instance FromJSON Symbol where
  parseJSON = fmap Symbol . parseJSON

newtype FeeRate = FeeRate { unFeeRate :: Double }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FeeRate where
  parseJSON (String s) = case reads (T.unpack s) of
    [(d, "")] -> return $ FeeRate d
    _         -> fail "Invalid fee rate string"
  parseJSON v = FeeRate <$> parseJSON v

data BookTicker = BookTicker
    { btSymbol   :: Symbol
    , btBidPrice :: Price
    , btBidQty   :: Double
    , btAskPrice :: Price
    , btAskQty   :: Double
    } deriving (Show, Eq, Generic)

instance FromJSON BookTicker where
  parseJSON = withObject "BookTicker" $ \o -> do
    symbol    <- o .: "symbol"
    bidPrice  <- o .: "bidPrice"
    bidQtyStr <- o .: "bidQty"
    askPrice  <- o .: "askPrice"
    askQtyStr <- o .: "askQty"
    bidQty <- case TR.double bidQtyStr of
      Right (d, "") -> return d
      _             -> fail "Invalid bidQty string"
    askQty <- case TR.double askQtyStr of
      Right (d, "") -> return d
      _             -> fail "Invalid askQty string"
    return $ BookTicker symbol bidPrice bidQty askPrice askQty

data TradeFee = TradeFee
    { tradeFeeSymbol  :: Symbol
    , takerFeeRate :: FeeRate
    } deriving (Show, Eq, Generic)

instance FromJSON TradeFee where
  parseJSON = withObject "TradeFee" $ \o -> do
    sym   <- o .: "symbol"
    taker <- o .: "takerCommission"
    return $ TradeFee sym taker

data AssetBalance = AssetBalance
    { abAsset :: Text
    , abFree  :: Double
    } deriving (Show, Generic)

instance FromJSON AssetBalance where
  parseJSON = withObject "AssetBalance" $ \o -> do
    asset   <- o .: "asset"
    freeStr <- o .: "free"
    free <- case TR.double freeStr of
      Right (d, "") -> return d
      _             -> fail "Invalid free balance string"
    return $ AssetBalance asset free

parseKnownAsset :: Text -> Maybe Asset
parseKnownAsset "BTC"  = Just BTC
parseKnownAsset "ETH"  = Just ETH
parseKnownAsset "USDT" = Just USDT
parseKnownAsset "BNB"  = Just BNB
parseKnownAsset _      = Nothing

data AccountInfo = AccountInfo
    { accountTakerCommission :: Int
    , accountBalances        :: Map Asset Double
    } deriving (Show, Eq, Generic)

instance FromJSON AccountInfo where
  parseJSON = withObject "AccountInfo" $ \o -> do
    taker   <- o .: "takerCommission"
    rawBals <- o .: "balances"
    let knownBals = Map.fromList
          [ (a, abFree b)
          | b <- rawBals
          , Just a <- [parseKnownAsset (abAsset b)]
          , abFree b > 0
          ]
    return $ AccountInfo taker knownBals

data OrderFill = OrderFill
    { ofPrice           :: Price
    , ofQty             :: Double
    , ofCommission      :: Double
    , ofCommissionAsset :: Asset
    } deriving (Show, Eq, Generic)

instance FromJSON OrderFill where
  parseJSON = withObject "OrderFill" $ \o -> do
    price           <- o .: "price"
    qtyStr          <- o .: "qty"
    commStr         <- o .: "commission"
    commissionAsset <- o .: "commissionAsset"
    qty <- case TR.double qtyStr of
      Right (d, "") -> return d
      _             -> fail "Invalid qty string"
    comm <- case TR.double commStr of
      Right (d, "") -> return d
      _             -> fail "Invalid commission string"
    return $ OrderFill price qty comm commissionAsset

data OrderResponse = OrderResponse
    { orStatus              :: Text
    , orExecutedQty         :: Double
    , orCummulativeQuoteQty :: Double
    , orFills               :: [OrderFill]
    } deriving (Show, Eq, Generic)

instance FromJSON OrderResponse where
  parseJSON = withObject "OrderResponse" $ \o -> do
    status  <- o .: "status"
    exqStr  <- o .: "executedQty"
    cqqStr  <- o .: "cummulativeQuoteQty"
    fills   <- o .: "fills"
    exq <- case TR.double exqStr of
      Right (d, "") -> return d
      _             -> fail "Invalid executedQty string"
    cqq <- case TR.double cqqStr of
      Right (d, "") -> return d
      _             -> fail "Invalid cummulativeQuoteQty string"
    return $ OrderResponse status exq cqq fills
