{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Binance.API.Types
    ( Asset(..)
    , Pair(..)
    , Symbol(..)
    , Price(..)
    , BookTicker(..)
    , TradeFee(..)
    , AccountInfo(..)
    , pairToSymbol
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

data Asset = BTC | ETH | USDT | BNB
  deriving (Show, Eq, Ord)

data Pair = Pair { base :: Asset, quote :: Asset }
  deriving (Show, Eq, Ord)

newtype Symbol = Symbol { unSymbol :: Text }
    deriving (Show, Eq, Ord, Generic)

assetToText :: Asset -> Text
assetToText = T.pack . show

pairToSymbol :: Pair -> Symbol
pairToSymbol (Pair b q) = Symbol $ assetToText b <> assetToText q

instance FromJSON Symbol where
    parseJSON = fmap Symbol . parseJSON

newtype Price = Price { unPrice :: Double }
    deriving (Show, Eq, Ord, Generic)

instance FromJSON Price where
    parseJSON (String s) = case reads (T.unpack s) of
        [(d, "")] -> return $ Price d
        _         -> fail "Invalid price string"
    parseJSON v = Price <$> parseJSON v

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
    , takerCommission :: Price
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
