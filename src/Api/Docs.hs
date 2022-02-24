{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Docs where

import Api.Tagger (TaggerAPI)

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

type DocsAPI = "docs" :> Get '[JSON] OpenApi

docsServer :: Server DocsAPI
docsServer = return $ toOpenApi (Proxy :: Proxy TaggerAPI)
  & info.title       .~ "Tagger api"
  & info.version     .~ "1.0.0"
  & info.description ?~ "Api endpoints for the tagger API"
