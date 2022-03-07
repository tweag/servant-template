{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.Id where

-- aeson
import Data.Aeson (ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- rel8
import Rel8 (DBEq, DBType)

-- uuid
import Data.UUID (UUID)

newtype Id a = Id {getUUID :: UUID}
  deriving newtype (DBEq, DBType, ToJSON, ToSchema)
