{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Infrastructure.Authentication.Login where

import Tagger.User (Password)

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- text
import Data.Text (Text)

data Login = Login
  { username :: Text
  , password :: Password
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, ToSchema)
