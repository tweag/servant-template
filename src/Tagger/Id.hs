{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.Id where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.UUID (UUID)
import Rel8 (DBEq, DBType)
import Servant.Auth.JWT (FromJWT, ToJWT)

-- |
-- An 'Id' is a newtype around a 'UUID' with a phantom type 'a' to keep track what the identifier is actually referring to
newtype Id a = Id {getUUID :: UUID}
  deriving stock (Eq, Ord, Show)
  deriving anyclass (FromJWT, ToJWT)
  deriving newtype (DBEq, DBType, FromJSON, ToJSON, ToSchema)
