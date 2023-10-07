module API (mkAPI, API, ApplicationAPI (..)) where

import API.Authentication qualified as Authentication
import API.Docs qualified as Docs
import API.Healthcheck qualified as Healthcheck
import API.Tagger qualified as Tagger
import App.Services (Services (..))
import AppM (AppM)
import GHC.Generics (Generic)
import Servant (ServerT)
import Servant.API (NamedRoutes, type (:>))
import Servant.API.Generic ((:-))
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Server.Generic (AsServerT)
import Tagger.Id (Id)
import Tagger.Repository.Content (ContentRepository)
import Tagger.User (User)

type API = NamedRoutes ApplicationAPI

-- | Collects all the API groups exposed by the application
data ApplicationAPI mode = ApplicationAPI
  { tagger :: mode :- Auth '[JWT] (Id User) :> NamedRoutes Tagger.API,
    docs :: mode :- Docs.API,
    healthcheck :: mode :- Healthcheck.API,
    authentication :: mode :- NamedRoutes Authentication.API
  }
  deriving stock (Generic)

-- | Setup all the application server, providing the services needed by the various endpoints
mkAPI :: Services AppM -> ServerT API AppM
mkAPI services =
  ApplicationAPI
    { tagger =
        authenticatedTaggerServer services.contentRepository,
      docs =
        Docs.api,
      healthcheck =
        Healthcheck.api,
      authentication =
        Authentication.api
          services.passwordManager
          services.authenticateUser
          services.userRepository
    }

-- | For the endpoints which actually require authentication, checks whether the request
-- provides a valid authentication token. Otherwise it returns a 401 response.
authenticatedTaggerServer :: ContentRepository AppM -> AuthResult (Id User) -> Tagger.API (AsServerT AppM)
authenticatedTaggerServer contentRepository = \case
  (Authenticated userId) -> Tagger.api userId contentRepository
  -- TODO Throw the error
  _ -> undefined -- throwAll err401
