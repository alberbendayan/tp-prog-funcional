module Notification.Types
    ( TelegramError(..)
    ) where

data TelegramError =
    TelegramSendError String
  | TelegramConfigError String
    deriving (Show, Eq)
