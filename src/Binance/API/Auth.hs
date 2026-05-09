module Binance.API.Auth
    ( signQueryString
    ) where

import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

hmacSha256Hex :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSha256Hex key msg =
    convertToBase Base16 (hmacGetDigest (hmac key msg :: HMAC SHA256))

signQueryString :: T.Text -> T.Text -> T.Text
signQueryString secret queryStr =
    TE.decodeUtf8 $ hmacSha256Hex (TE.encodeUtf8 secret) (TE.encodeUtf8 queryStr)
