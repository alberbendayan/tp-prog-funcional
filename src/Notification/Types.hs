module Notification.Types
    ( TelegramError(..)
    ) where

import Data.Text (Text)

data TelegramError =
    TelegramSendError Text
  | TelegramConfigError Text
    deriving (Show, Eq)
