{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.Tag where

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

import Data.Text (Text)

-- |
-- A 'Tag' is a newtype wrapper around some 'Text', used to index a 'Tagger.Content.Content'
newtype Tag = Tag { name :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToSchema, FromJSON, ToJSON)
