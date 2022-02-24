{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Tagger.Content where

import Tagger.Tag (Tag)

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- text
import Data.Text (Text)

data Content = Content
  { _content :: Text
  , _tags :: [Tag]
  }
  deriving (Generic, ToSchema, FromJSON, ToJSON)
