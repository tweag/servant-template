{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Docs where

import Api.Authentication (AuthenticationAPI)
import Api.Tagger (TaggerAPI)
import Control.Lens ((&), (.~), (?~))
import Data.OpenApi (OpenApi, description, info, title, version)
import Data.Proxy (Proxy (Proxy))
import Servant (Get, JSON, Server, (:>))
import Servant.OpenApi (toOpenApi)

-- |
-- A single endpoint to expose the OpenAPI documentation of the application
type DocsAPI = "docs" :> Get '[JSON] OpenApi

docsServer :: Server DocsAPI
docsServer =
  return $
    toOpenApi (Proxy :: Proxy TaggerAPI) <> toOpenApi (Proxy :: Proxy AuthenticationAPI)
      & info . title .~ "Tagger api"
      & info . version .~ "1.0.0"
      & info . description ?~ "Api endpoints for the tagger API"
