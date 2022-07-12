{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tagger where

import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Servant (Handler)
import Servant.API (Get, JSON, Post, QueryParams, ReqBody, type (:>))
import Servant.API.Generic ((:-))
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server.Generic (AsServer)
import Tagger.Content (Content)
import Tagger.ContentRepository (ContentRepository (addContentWithTags, selectUserContentsByTags))
import Tagger.Id (Id)
import Tagger.Owned (Owned)
import Tagger.Tag (Tag)
import Tagger.User (User)
import Prelude hiding (getContents)

-- |
-- The main endpoints of the application API
data TaggerAPI mode = TaggerAPI
  { -- | Add a new 'Content'
    addContent :: mode :- "add-content" :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] (Id (Content Tag)),
    -- | Retrieve all the 'User' 'Content's indexed by the provided 'Tag's
    getContents :: mode :- "get-contents" :> QueryParams "tag" Tag :> Get '[JSON] [Owned (Content Tag)]
  }
  deriving stock (Generic)

instance HasOpenApi TaggerAPI where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy ("add-content" :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] (Id (Content Tag))))
      <> toOpenApi (Proxy :: Proxy ("get-contents" :> QueryParams "tag" Tag :> Get '[JSON] [Owned (Content Tag)]))

taggerServer :: Id User -> ContentRepository Handler -> TaggerAPI AsServer
taggerServer userId contentRepository =
  TaggerAPI
    { addContent = addContentWithTags contentRepository userId,
      getContents = selectUserContentsByTags contentRepository userId
    }
