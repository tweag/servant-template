{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tagger.User where

import Tagger.EncryptedPassword (EncryptedPassword)

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (ToJSON(toJSON), (.=), object)


-- openapi3
import Data.OpenApi (ToSchema)

-- text
import Data.Text (Text)

-- |
-- A 'User' contains a 'Text' and an 'EncryptedPassword'
data User = User
  { name :: Text
  , password :: EncryptedPassword
  }
  deriving stock (Eq, Show, Read, Generic)

-- |
-- We need to be careful to hide the password (even if it is encrypted) when we expose an 'User'
instance ToJSON User where
  toJSON User{name} = object ["name" .= name]

instance ToSchema User
