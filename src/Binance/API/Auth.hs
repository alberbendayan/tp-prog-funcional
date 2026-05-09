module Binance.API.Auth
    ( signQueryString
    ) where

import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

signQueryString :: T.Text -> T.Text -> T.Text
signQueryString secret queryStr =
    let secretBS  = TE.encodeUtf8 secret
        msgBS     = TE.encodeUtf8 queryStr
        digest    = hmacGetDigest (hmac secretBS msgBS :: HMAC SHA256)
        hexBytes  = convertToBase Base16 digest :: BS.ByteString
    in TE.decodeUtf8 hexBytes
