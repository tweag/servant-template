{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.Id where

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- rel8
import Rel8 (DBEq, DBType)

-- servant-auth
import Servant.Auth.JWT (FromJWT, ToJWT)

-- uuid
import Data.UUID (UUID)

newtype Id a = Id {getUUID :: UUID}
  deriving stock (Eq, Ord)
  deriving anyclass (FromJWT, ToJWT)
  deriving newtype (DBEq, DBType, FromJSON, ToJSON, ToSchema)
