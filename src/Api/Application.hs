{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Application where

import Api.Docs (DocsAPI, docsServer)
import Api.Healthcheck (HealthcheckAPI, healthcheckServer)
import Api.Tagger (TaggerAPI, taggerServer)

-- base
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)

-- servant
import Servant.API (NamedRoutes)
import Servant.API.Generic ((:-))

-- servant-server
import Servant (serve)
import Servant.Server.Generic (AsServer)

-- wai
import Network.Wai (Application)

-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

type API = NamedRoutes ApplicationAPI

data ApplicationAPI mode = ApplicationAPI
  { tagger      :: mode :- NamedRoutes TaggerAPI
  , docs        :: mode :- DocsAPI
  , healthcheck :: mode :- HealthcheckAPI
  } deriving (Generic)

server :: ApplicationAPI AsServer
server = ApplicationAPI
  { tagger      = taggerServer
  , docs        = docsServer
  , healthcheck = healthcheckServer
  }

app :: Application
app = logStdoutDev (serve
  (Proxy :: Proxy API)
  server)
