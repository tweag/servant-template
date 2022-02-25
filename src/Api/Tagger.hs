{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tagger where

import Tagger.Content (Content)
import Tagger.Tag (Tag)

-- base
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Prelude hiding (getContents)

-- servant
import Servant.API (type (:>), Get, Post, JSON, ReqBody)
import Servant.API.Generic ((:-))

-- servant-openapi3
import Servant.OpenApi (HasOpenApi(toOpenApi))

-- servant-server
import Servant (Handler)
import Servant.Server.Generic (AsServer)

-- uuid
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

data TaggerAPI mode = TaggerAPI
  { addContent  :: mode :- "add-content"  :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] UUID
  , getContents :: mode :- "get-contents" :> ReqBody '[JSON] [Tag]         :> Get  '[JSON] [Content Tag]
  }
  deriving stock Generic

instance HasOpenApi TaggerAPI where
  toOpenApi _ = mempty -- TODO: generate this automatically

addContentHandler :: Content Tag -> Handler UUID
addContentHandler _ = liftIO nextRandom

getContentsHandler :: [Tag] -> Handler [Content Tag]
getContentsHandler _ = pure []

taggerServer :: TaggerAPI AsServer
taggerServer = TaggerAPI
  { addContent  = addContentHandler
  , getContents = getContentsHandler
  }
