{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Binance.API.Types
    ( Asset(..)
    , Pair(..)
    , Symbol(..)
    , Price(..)
    , TickerPrice(..)
    , BookTicker(..)
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

instance ToJSON Symbol where
    toJSON = toJSON . unSymbol

newtype Price = Price { unPrice :: Double }
    deriving (Show, Eq, Ord, Generic)

instance FromJSON Price where
    parseJSON (String s) = case reads (T.unpack s) of
        [(d, "")] -> return $ Price d
        _         -> fail "Invalid price string"
    parseJSON v = Price <$> parseJSON v

instance ToJSON Price where
    toJSON = toJSON . unPrice

data TickerPrice = TickerPrice
    { tpSymbol :: Symbol
    , tpPrice  :: Price
    } deriving (Show, Eq, Generic)

instance FromJSON TickerPrice where
    parseJSON = withObject "TickerPrice" $ \o -> do
        symbol <- o .: "symbol"
        price  <- o .: "price"
        return $ TickerPrice symbol price

instance ToJSON TickerPrice where
    toJSON tp = object
        [ "symbol" .= tpSymbol tp
        , "price"  .= tpPrice tp
        ]

data BookTicker = BookTicker
    { btSymbol   :: Symbol
    , btBidPrice :: Price
    , btBidQty   :: Double
    , btAskPrice :: Price
    , btAskQty   :: Double
    } deriving (Show, Eq, Generic)

instance FromJSON BookTicker where
    parseJSON = withObject "BookTicker" $ \o -> do
        symbol   <- o .: "symbol"
        bidPrice <- o .: "bidPrice"
        bidQtyStr <- o .: "bidQty"
        askPrice <- o .: "askPrice"
        askQtyStr <- o .: "askQty"
        -- Parsear bidQty y askQty como String y convertir a Double
        bidQty <- case reads (T.unpack bidQtyStr) of
            [(d, "")] -> return d
            _         -> fail "Invalid bidQty string"
        askQty <- case reads (T.unpack askQtyStr) of
            [(d, "")] -> return d
            _         -> fail "Invalid askQty string"
        return $ BookTicker symbol bidPrice bidQty askPrice askQty

instance ToJSON BookTicker where
    toJSON bt = object
        [ "symbol"   .= btSymbol bt
        , "bidPrice" .= btBidPrice bt
        , "bidQty"   .= btBidQty bt
        , "askPrice" .= btAskPrice bt
        , "askQty"   .= btAskQty bt
        ]
