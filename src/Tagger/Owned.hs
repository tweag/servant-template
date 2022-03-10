{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Owned where

import Tagger.Id (Id)
import Tagger.User (User)

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

data Owned a = Owned
  { _userId  :: Id User
  , _content :: a
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON a => FromJSON (Owned a)

instance ToJSON a => ToJSON (Owned a)

instance ToSchema a => ToSchema (Owned a)
