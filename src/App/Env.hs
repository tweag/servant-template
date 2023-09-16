module App.Env (Env (..), Handles (..)) where

import API.Config (Config (..))
import GHC.Generics (Generic)
import Infrastructure.Database qualified as DB
import Infrastructure.Logging.Logger qualified as Logger
import Infrastructure.SystemTime qualified as SystemTime
import Tagger.JSONWebKey qualified as JWK

data Env = Env
  { handles :: Handles,
    config :: Config,
    jwkKey :: JWK.JWK
  }
  deriving (Generic)

-- |
-- Aggregates all effects needed by the app
data Handles = Handles
  { systemTime :: SystemTime.Handle,
    logger :: Logger.Handle,
    database :: DB.Handle
  }
  deriving (Generic)
