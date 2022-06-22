{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Application where

import Api.AppServices (AppServices(..))
import Api.Authentication (AuthenticationAPI, authenticationServer)
import Api.Docs (DocsAPI, docsServer)
import Api.Root (RootAPI, rootServer)
import Api.Healthcheck (HealthcheckAPI, healthcheckServer)
import Api.Tagger (TaggerAPI, taggerServer)
import Tagger.ContentRepository (ContentRepository)
import Tagger.Id (Id)
import Tagger.User (User)

-- base
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)

-- servant
import Servant.API (NamedRoutes, type (:>))
import Servant.API.Generic ((:-))

-- servant-auth
import Servant.Auth (Auth, JWT)

-- servant-auth-server
import Servant.Auth.Server (defaultCookieSettings, AuthResult (Authenticated), ThrowAll (throwAll))

-- servant-server
import Servant (Handler, serveWithContext, Context ((:.), EmptyContext), err401)
import Servant.Server.Generic (AsServer)

-- wai
import Network.Wai (Application)

type API = NamedRoutes ApplicationAPI

-- |
-- Collects all the API groups exposed by the application
data ApplicationAPI mode = ApplicationAPI
  { root           :: mode :- RootAPI
  , tagger         :: mode :- Auth '[JWT] (Id User) :> NamedRoutes TaggerAPI
  , docs           :: mode :- DocsAPI
  , healthcheck    :: mode :- HealthcheckAPI
  , authentication :: mode :- NamedRoutes AuthenticationAPI
  }
  deriving stock Generic

-- |
-- For the endpoints which actually require authentication, checks whether the request provides a valid authentication token.
-- Otherwise it returns a 401 response
authenticatedTaggerServer :: ContentRepository Handler -> AuthResult (Id User) -> TaggerAPI AsServer
authenticatedTaggerServer contentRepository = \case
  (Authenticated userId) -> taggerServer userId contentRepository
  _                      -> throwAll err401

-- |
-- Setup all the application server, providing the services needed by the various endpoints
server :: AppServices -> ApplicationAPI AsServer
server AppServices{passwordManager, contentRepository, userRepository, authenticateUser} = ApplicationAPI
  { root = rootServer
  , tagger         = authenticatedTaggerServer contentRepository
  , docs           = docsServer
  , healthcheck    = healthcheckServer
  , authentication = authenticationServer passwordManager authenticateUser userRepository
  }


app :: AppServices -> Application
app appServices = serveWithContext
  (Proxy :: Proxy API)
  (defaultCookieSettings :. jwtSettings appServices :. EmptyContext)
  (server appServices)
