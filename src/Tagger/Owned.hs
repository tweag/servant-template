{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Owned where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Tagger.Id (Id)
import Tagger.User (User)

-- |
-- 'Owned' is a data type used to associate a 'User' to a content via its 'Id'
data Owned a = Owned
  { userId :: Id User,
    content :: a
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON a => FromJSON (Owned a)

instance ToJSON a => ToJSON (Owned a)

instance ToSchema a => ToSchema (Owned a)
