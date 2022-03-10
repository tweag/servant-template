{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Infrastructure.Authentication.Login where

-- base
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON (parseJSON), ToJSON(toJSON))

-- bytestring
import Data.ByteString (ByteString)

-- openapi3
import Data.OpenApi (ToSchema(declareNamedSchema))

-- text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

newtype Password = Password {asBytestring :: ByteString}

instance FromJSON Password where
  parseJSON json = Password . encodeUtf8 <$> parseJSON json

instance ToJSON Password where
  toJSON (Password s) = toJSON $ decodeUtf8 s

instance ToSchema Password where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

data Login = Login
  { username :: Text
  , password :: Password
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, ToSchema)
