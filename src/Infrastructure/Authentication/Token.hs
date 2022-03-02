module Infrastructure.Authentication.Token where

-- aeson
import Data.Aeson (ToJSON(toJSON), Value(String))

-- bytestring
import Data.ByteString.Lazy (ByteString, toStrict)

-- text
import Data.Text.Encoding (decodeUtf8)

newtype Token = Token ByteString

instance ToJSON Token where
  toJSON (Token bs) = String . decodeUtf8 $ toStrict bs
