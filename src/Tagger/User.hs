{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
  { _name :: Text
  , _password :: EncryptedPassword
  }
  deriving stock (Eq, Show, Read, Generic)

-- |
-- We need to be careful to hide the password (even if it is encrypted) when we expose an 'User'
instance ToJSON User where
  toJSON User{_name} = object ["_name" .= _name]

instance ToSchema User
