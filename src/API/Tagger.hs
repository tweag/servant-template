module API.Tagger (API (..), api) where

import AppM (AppM)
import GHC.Generics (Generic)
import Servant.API (Get, JSON, Post, QueryParams, ReqBody, type (:>))
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServerT)
import Tagger.Content (Content)
import Tagger.Id (Id)
import Tagger.Owned (Owned)
import Tagger.Repository.Content (ContentRepository (addContentWithTags, selectUserContentsByTags))
import Tagger.Tag (Tag)
import Tagger.User (User)

-- |
-- The main endpoints of the application API
data API mode = API
  { -- | Add a new 'Content'
    addContent :: mode :- "add-content" :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] (Id (Content Tag)),
    -- | Retrieve all the 'User' 'Content's indexed by the provided 'Tag's
    getContents :: mode :- "get-contents" :> QueryParams "tag" Tag :> Get '[JSON] [Owned (Content Tag)]
  }
  deriving stock (Generic)

api :: Id User -> ContentRepository AppM -> API (AsServerT AppM)
api userId contentRepository =
  API
    { addContent = addContentImpl userId contentRepository,
      getContents = getContentsImpl userId contentRepository
    }

addContentImpl :: Id User -> ContentRepository AppM -> Content Tag -> AppM (Id (Content Tag))
addContentImpl userId contentRepository = contentRepository.addContentWithTags userId

getContentsImpl :: Id User -> ContentRepository AppM -> [Tag] -> AppM [Owned (Content Tag)]
getContentsImpl userId contentRepository = contentRepository.selectUserContentsByTags userId
