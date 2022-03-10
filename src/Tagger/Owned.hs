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

-- |
-- 'Owned' is a data type used to associate a 'User' to a content via its 'Id'
data Owned a = Owned
  { _userId  :: Id User
  , _content :: a
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON a => FromJSON (Owned a)

instance ToJSON a => ToJSON (Owned a)

instance ToSchema a => ToSchema (Owned a)
