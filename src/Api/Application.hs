{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Application where

import Api.AppServices (AppServices (..))
import Api.Authentication (AuthenticationAPI, authenticationServer)
import Api.AuthenticationWeb (AuthenticationWeb, authenticationServerWeb)
import Api.Docs (DocsAPI, docsServer)
import Api.Healthcheck (HealthcheckAPI, healthcheckServer)
import Api.Root (RootAPI, rootServer)
import Api.Tagger (TaggerAPI, taggerServer)
import Api.TaggerWeb (TaggerWeb, taggerServerWeb)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant (Context (EmptyContext, (:.)), Handler, err401, serveWithContext)
import Servant.API (NamedRoutes, type (:>))
import Servant.API.Generic ((:-))
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll))
import Servant.Server.Generic (AsServer)
import Tagger.ContentRepository (ContentRepository)
import Tagger.Id (Id)
import Tagger.User (User)

type API = NamedRoutes ApplicationAPI

-- |
-- Collects all the API groups exposed by the application
data ApplicationAPI mode = ApplicationAPI
  { root :: mode :- RootAPI,
    tagger :: mode :- Auth '[JWT] (Id User) :> NamedRoutes TaggerAPI,
    taggerWeb :: mode :- Auth '[JWT] (Id User) :> NamedRoutes TaggerWeb,
    authenticationWeb :: mode :- NamedRoutes AuthenticationWeb,
    authentication :: mode :- NamedRoutes AuthenticationAPI,
    docs :: mode :- DocsAPI,
    healthcheck :: mode :- HealthcheckAPI
  }
  deriving stock (Generic)

-- |
-- For the endpoints which actually require authentication, checks whether the request provides a valid authentication token.
-- Otherwise it returns a 401 response
authenticatedTaggerServer :: ContentRepository Handler -> AuthResult (Id User) -> TaggerAPI AsServer
authenticatedTaggerServer contentRepository = \case
  (Authenticated userId) -> taggerServer userId contentRepository
  _ -> throwAll err401

-- For the endpoints which actually require authentication, checks whether the request provides a valid authentication token.
-- Otherwise it returns a 401 response
authenticatedTaggerWebServer :: ContentRepository Handler -> AuthResult (Id User) -> TaggerWeb AsServer
authenticatedTaggerWebServer contentRepository = \case
  (Authenticated userId) -> taggerServerWeb userId contentRepository
  _ -> throwAll err401

-- |
-- Setup all the application server, providing the services needed by the various endpoints
server :: AppServices -> ApplicationAPI AsServer
server AppServices {passwordManager, contentRepository, userRepository, authenticateUser} =
  ApplicationAPI
    { root = rootServer,
      tagger = authenticatedTaggerServer contentRepository,
      docs = docsServer,
      healthcheck = healthcheckServer,
      authentication = authenticationServer passwordManager authenticateUser userRepository,
      taggerWeb = authenticatedTaggerWebServer contentRepository,
      authenticationWeb = authenticationServerWeb passwordManager authenticateUser userRepository
    }

app :: AppServices -> Application
app appServices =
  serveWithContext
    (Proxy :: Proxy API)
    (cookieSettings appServices :. jwtSettings appServices :. EmptyContext)
    (server appServices)
