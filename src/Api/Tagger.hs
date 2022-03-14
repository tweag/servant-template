{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tagger where

import Tagger.Content (Content)
import Tagger.ContentRepository (ContentRepository(selectUserContentsByTags, addContentWithTags))
import Tagger.Id (Id)
import Tagger.Owned (Owned)
import Tagger.Tag (Tag)
import Tagger.User (User)

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

-- |
-- The main endpoints of the application API
data TaggerAPI mode = TaggerAPI
  { addContent  :: mode :- "add-content"  :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] (Id (Content Tag))    -- ^ Add a new 'Content'
  , getContents :: mode :- "get-contents" :> ReqBody '[JSON] [Tag]         :> Get  '[JSON] [Owned (Content Tag)] -- ^ Retrieve all the 'User' 'Content's indexed by the provided 'Tag's
  }
  deriving stock Generic

instance HasOpenApi TaggerAPI where
  toOpenApi _
    =  toOpenApi (Proxy :: Proxy ("add-content"  :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] (Id (Content Tag))))
    <> toOpenApi (Proxy :: Proxy ("get-contents" :> ReqBody '[JSON] [Tag]         :> Get  '[JSON] [Owned (Content Tag)]))

taggerServer :: Id User -> ContentRepository Handler -> TaggerAPI AsServer
taggerServer userId contentRepository = TaggerAPI
  { addContent  = addContentWithTags contentRepository userId
  , getContents = selectUserContentsByTags contentRepository userId
  }
