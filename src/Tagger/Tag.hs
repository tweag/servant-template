{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.Tag where

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

import Data.Text (Text)

newtype Tag = Tag { _name :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToSchema, FromJSON, ToJSON)
