{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Tagger.User where

-- base
import Data.Data (Proxy(Proxy))
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), (.=), object)

-- bytestring
import Data.ByteString (ByteString)

-- openapi3
import Data.OpenApi (ToSchema(declareNamedSchema))

-- text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

newtype Password = Password {asBytestring :: ByteString}
  deriving stock (Eq, Show, Read, Generic)

instance FromJSON Password where
  parseJSON json = Password . encodeUtf8 <$> parseJSON json

instance ToJSON Password where
  toJSON (Password s) = toJSON $ decodeUtf8 s

instance ToSchema Password where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

data User = User
  { _name :: Text
  , _password :: Password
  }
  deriving stock (Eq, Show, Read, Generic)

instance ToJSON User where
  toJSON (User name _) = object ["_name" .= name]

instance ToSchema User
