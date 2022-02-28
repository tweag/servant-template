{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Application where

import Api.Docs (DocsAPI, docsServer)
import Api.Healthcheck (HealthcheckAPI, healthcheckServer)
import Api.Tagger (TaggerAPI, taggerServer)
import Tagger.ContentRepository (ContentRepository)

-- base
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)

-- servant
import Servant.API (NamedRoutes)
import Servant.API.Generic ((:-))

-- servant-server
import Servant (serve, Handler)
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
  }
  deriving stock Generic

server :: ContentRepository Handler -> ApplicationAPI AsServer
server contentRepository = ApplicationAPI
  { tagger      = taggerServer contentRepository
  , docs        = docsServer
  , healthcheck = healthcheckServer
  }

app :: ContentRepository Handler -> Application
app contentRepository = logStdoutDev $ serve
  (Proxy :: Proxy API)
  (server contentRepository)