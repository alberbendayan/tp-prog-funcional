module Binance.API.Auth
    ( signQueryString
    ) where

import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

signQueryString :: String -> String -> String
signQueryString secret queryStr =
    let secretBS  = BSC.pack secret
        msgBS     = BSC.pack queryStr
        digest    = hmacGetDigest (hmac secretBS msgBS :: HMAC SHA256)
        hexBytes  = convertToBase Base16 digest :: BS.ByteString
    in BSC.unpack hexBytes
