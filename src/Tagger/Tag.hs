{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.Tag where

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

import Data.Text (Text)

newtype Tag = Tag Text
  deriving (ToSchema, FromJSON, ToJSON)
