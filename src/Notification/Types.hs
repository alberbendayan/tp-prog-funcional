module Notification.Types
    ( TelegramError(..)
    , Notification(..)
    ) where

import Binance.API.Types (Symbol, Price, TickerPrice)

data TelegramError =
    TelegramSendError String
  | TelegramConfigError String
    deriving (Show, Eq)

data Notification = 
    OpportunityDetected TickerPrice
  | PriceAlert Symbol Price
    deriving (Show, Eq)
