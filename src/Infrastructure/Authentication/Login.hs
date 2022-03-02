{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Infrastructure.Authentication.Login where

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON)

-- text
import Data.Text (Text)

data Login = Login
  { username :: Text
  , password :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON
