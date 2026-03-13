module Exchange.Interface where

import Bot.Domain (MarketSnapshot)
import Binance.API.Types (Asset)

data ExchangeError
    = ExchangeConnError String
    | ExchangeFetchError String
    deriving (Show, Eq)

class Exchange e where
    checkConnectivity   :: e -> IO (Either ExchangeError Bool)
    fetchMarketSnapshot :: e -> [Asset] -> IO (Either ExchangeError MarketSnapshot)
