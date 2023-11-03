module API.Docs (API, api) where

import API.Authentication qualified as Authentication
import API.Content qualified as Content
import AppM (AppM)
import Data.OpenApi (OpenApi)
import Data.Proxy (Proxy (Proxy))
import Optics
import Servant (Get, JSON, NamedRoutes, ServerT)
import Servant.OpenApi (toOpenApi)

-- |
-- A single endpoint to expose the OpenAPI documentation of the application
type API = Get '[JSON] OpenApi

api :: ServerT API AppM
api =
  return $
    toOpenApi (Proxy :: Proxy (NamedRoutes Content.API)) <> toOpenApi (Proxy :: Proxy (NamedRoutes Authentication.API))
      & #info % #title .~ "Tagger api"
      & #info % #version .~ "1.0.0"
      & #info % #description ?~ "API endpoints for the tagger API"
