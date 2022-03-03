{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.User where

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))

-- bytestring
import Data.ByteString (ByteString)

-- servant-auth
import Servant.Auth.JWT (ToJWT, FromJWT)

-- text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

newtype Password = Password {asBytestring :: ByteString}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJWT, FromJWT)

instance FromJSON Password where
  parseJSON json = Password . encodeUtf8 <$> parseJSON json

instance ToJSON Password where
  toJSON (Password s) = toJSON $ decodeUtf8 s

data User = User
  { _name :: Text
  , _password :: Password
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, ToJWT, FromJSON, FromJWT)
