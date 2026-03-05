{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Binance.API.Types
    ( Symbol(..)
    , Price(..)
    , TickerPrice(..)
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

newtype Symbol = Symbol { unSymbol :: Text }
    deriving (Show, Eq, Ord, Generic)

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
