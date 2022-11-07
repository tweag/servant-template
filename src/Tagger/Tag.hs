{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.Tag where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToParamSchema, ToSchema)

-- text
import Data.Text (Text)
import Servant (FromHttpApiData, ToHttpApiData)

-- |
-- A 'Tag' is a newtype wrapper around some 'Text', used to index a 'Tagger.Content.Content'
newtype Tag = Tag {name :: Text}
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, ToHttpApiData, ToParamSchema, ToSchema, FromJSON, ToJSON)
