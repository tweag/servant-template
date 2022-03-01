{-# LANGUAGE DeriveGeneric #-}

module Tagger.User where

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (ToJSON, FromJSON)

-- servant-auth
import Servant.Auth.JWT (ToJWT, FromJWT)

-- text
import Data.Text (Text)

newtype User = User {name :: Text}
  deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User
