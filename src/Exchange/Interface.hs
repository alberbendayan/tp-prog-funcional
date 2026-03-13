module Exchange.Interface where

import Bot.Domain (MarketSnapshot)
import Binance.API.Types (Asset)
import Control.Monad.IO.Class (MonadIO)

data ExchangeError
    = ExchangeConnError String
    | ExchangeFetchError String
    deriving (Show, Eq)


class Exchange e where
    checkConnectivity
        :: MonadIO m
        => e -> m (Either ExchangeError Bool)

    fetchMarketSnapshot
        :: MonadIO m
        => e -> [Asset] -> m (Either ExchangeError MarketSnapshot)
