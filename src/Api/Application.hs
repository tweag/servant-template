{-# LANGUAGE TypeOperators #-}

module Api.Application where

import Api.Docs (DocsAPI, docsServer)
import Api.Tagger (TaggerAPI, taggerServer)

-- base
import Data.Proxy (Proxy(..))

-- servant
import Servant.API (type (:<|>)(..))

-- servant-server
import Servant (serve)

-- wai
import Network.Wai (Application)

-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

type API = TaggerAPI :<|> DocsAPI

app :: Application
app = logStdoutDev (serve (Proxy :: Proxy API) (taggerServer :<|> docsServer))
