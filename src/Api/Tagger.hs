{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tagger where

import Tagger.Content (Content)
import Tagger.ContentRepository (ContentRepository(selectContentsByTags, addContentWithTags))
import Tagger.Tag (Tag)

-- base
import Data.Proxy (Proxy(Proxy))
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

data TaggerAPI mode = TaggerAPI
  { addContent  :: mode :- "add-content"  :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] UUID
  , getContents :: mode :- "get-contents" :> ReqBody '[JSON] [Tag]         :> Get  '[JSON] [Content Tag]
  }
  deriving stock Generic

instance HasOpenApi TaggerAPI where
  toOpenApi _
    =  toOpenApi (Proxy :: Proxy ("add-content"  :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] UUID))
    <> toOpenApi (Proxy :: Proxy ("get-contents" :> ReqBody '[JSON] [Tag]         :> Get  '[JSON] [Content Tag]))

taggerServer :: ContentRepository Handler -> TaggerAPI AsServer
taggerServer contentRepository = TaggerAPI
  { addContent  = addContentWithTags contentRepository
  , getContents = selectContentsByTags contentRepository
  }
