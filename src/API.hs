module API (mkAPI, API, ApplicationAPI (..), PublicRoutes (..), SecureRoutes (..)) where

import API.Authentication qualified as Authentication
import API.Content qualified as Content
import API.Docs qualified as Docs
import API.Healthcheck qualified as Healthcheck
import App.Services (Services (..))
import AppM (AppM)
import GHC.Generics (Generic)
import Servant (ServerT)
import Servant.API (NamedRoutes, type (:>))
import Servant.API.Generic ((:-))
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server (AuthResult (Authenticated))
import Tagger.Id (Id)
import Tagger.User (User)

type API = NamedRoutes ApplicationAPI

type WithAuth = Auth '[JWT] (Id User)

data ApplicationAPI mode = ApplicationAPI
  { public :: mode :- NamedRoutes PublicRoutes,
    secure :: mode :- WithAuth :> NamedRoutes SecureRoutes
  }
  deriving (Generic)

-- | Routes without any authentication required.
data PublicRoutes mode = PublicRoutes
  { docs :: mode :- "docs" :> Docs.API,
    healthcheck :: mode :- "healthcheck" :> Healthcheck.API,
    authentication :: mode :- NamedRoutes Authentication.API
  }
  deriving (Generic)

-- | Routes with required authentication.
newtype SecureRoutes mode = SecureRoutes
  { content :: mode :- NamedRoutes Content.API
  }
  deriving (Generic)

-- | Setup the application server, providing the services needed by the various
-- endpoints
mkAPI :: Services AppM -> ServerT API AppM
mkAPI services =
  ApplicationAPI
    { public =
        PublicRoutes
          { docs =
              Docs.api,
            healthcheck =
              Healthcheck.api,
            authentication =
              Authentication.api
                services.passwordManager
                services.authenticateUser
                services.userRepository
          },
      secure = \case
        (Authenticated userId) ->
          SecureRoutes
            { content = Content.api userId services.contentRepository
            }
        _ -> undefined -- throwAll err401
    }
