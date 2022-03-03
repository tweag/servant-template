module Infrastructure.Authentication.Token where

-- base
import Data.Data (Proxy(Proxy))

-- aeson
import Data.Aeson (ToJSON(toJSON), Value(String))

-- bytestring
import Data.ByteString.Lazy (ByteString, toStrict)

-- openapi3
import Data.OpenApi (ToSchema(declareNamedSchema))

-- text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

newtype Token = Token ByteString

instance ToJSON Token where
  toJSON (Token bs) = String . decodeUtf8 $ toStrict bs

instance ToSchema Token where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
