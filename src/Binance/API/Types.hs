{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Tipos específicos del wire format de Binance. Los conceptos de mercado compartidos
-- ('Asset', 'Pair', 'Price', 'MarketOrderQty') viven en "Bot.Domain".
module Binance.API.Types
    ( -- Re-exportados desde Bot.Domain
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
    bidQty <- case reads (T.unpack bidQtyStr) of
      [(d, "")] -> return d
      _         -> fail "Invalid bidQty string"
    askQty <- case reads (T.unpack askQtyStr) of
      [(d, "")] -> return d
      _         -> fail "Invalid askQty string"
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

-- | takerCommission en basis points (ej. 10 = 0.001)
data AccountInfo = AccountInfo
    { accountTakerCommission :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON AccountInfo where
  parseJSON = withObject "AccountInfo" $ \o ->
    AccountInfo <$> o .: "takerCommission"

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
    qty <- case reads (T.unpack qtyStr) of
      [(d, "")] -> return d
      _         -> fail "Invalid qty string"
    comm <- case reads (T.unpack commStr) of
      [(d, "")] -> return d
      _         -> fail "Invalid commission string"
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
    exq <- case reads (T.unpack exqStr) of
      [(d, "")] -> return d
      _         -> fail "Invalid executedQty string"
    cqq <- case reads (T.unpack cqqStr) of
      [(d, "")] -> return d
      _         -> fail "Invalid cummulativeQuoteQty string"
    return $ OrderResponse status exq cqq fills
