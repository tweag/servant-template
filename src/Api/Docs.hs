{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Docs where

import Api.Tagger (TaggerAPI)
import Api.Authentication (AuthenticationAPI)

-- base
import Data.Proxy (Proxy(Proxy))

-- lens
import Control.Lens ((&), (.~), (?~))

-- openapi3
import Data.OpenApi (OpenApi, info, description, title, version)

-- servant-openapi3
import Servant.OpenApi (toOpenApi)

-- servant-server
import Servant ((:>), Get, JSON, Server)

-- |
-- A single endpoint to expose the OpenAPI documentation of the application
type DocsAPI = "docs" :> Get '[JSON] OpenApi

docsServer :: Server DocsAPI
docsServer = return $ toOpenApi (Proxy :: Proxy TaggerAPI) <> toOpenApi (Proxy :: Proxy AuthenticationAPI)
  & info.title       .~ "Tagger api"
  & info.version     .~ "1.0.0"
  & info.description ?~ "Api endpoints for the tagger API"
