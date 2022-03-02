{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, ToJWT, FromJSON, FromJWT)
