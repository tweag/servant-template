module API.Docs (API, api) where

import API.Authentication qualified as Authentication
import API.Tagger qualified as Tagger
import Data.OpenApi (OpenApi)
import Data.Proxy (Proxy (Proxy))
import Optics
import Servant (Get, JSON, NamedRoutes, Server, (:>))
import Servant.OpenApi (toOpenApi)

-- |
-- A single endpoint to expose the OpenAPI documentation of the application
type API = "docs" :> Get '[JSON] OpenApi

api :: Server API
api =
  return $
    toOpenApi (Proxy :: Proxy (NamedRoutes Tagger.API)) <> toOpenApi (Proxy :: Proxy (NamedRoutes Authentication.API))
      & #info % #title .~ "Tagger api"
      & #info % #version .~ "1.0.0"
      & #info % #description ?~ "API endpoints for the tagger API"
