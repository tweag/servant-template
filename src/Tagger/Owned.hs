{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Owned where

import Tagger.Id (Id)
import Tagger.User (User)

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

data Owned a = Owned
  { _user    :: User
  , _userId  :: Id User
  , _content :: a
  }
  deriving stock Generic

instance ToJSON a => ToJSON (Owned a)

instance ToSchema a => ToSchema (Owned a)
