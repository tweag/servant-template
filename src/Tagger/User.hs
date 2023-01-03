{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tagger.User where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Tagger.EncryptedPassword (EncryptedPassword)

-- |
-- A 'User' contains a 'Text' and an 'EncryptedPassword'
data User = User
  { name :: Text,
    password :: EncryptedPassword
  }
  deriving stock (Eq, Show, Read, Generic)

-- |
-- We need to be careful to hide the password (even if it is encrypted) when we expose an 'User'
instance ToJSON User where
  toJSON User {name} = object ["name" .= name]

instance ToSchema User
