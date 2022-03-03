{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Infrastructure.Authentication.Login where

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- text
import Data.Text (Text)

data Login = Login
  { username :: Text
  , password :: Text
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToSchema)
