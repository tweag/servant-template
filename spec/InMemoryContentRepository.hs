module InMemoryContentRepository where

import Tagger.Content (Content, hasAllTags)
import Tagger.ContentRepository (ContentRepository(..))
import Tagger.Id (Id)
import Tagger.Owned (Owned)
import Tagger.Tag (Tag)
import Tagger.User (User)

-- base
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (TVar, atomically, readTVar)
import Prelude hiding (filter)

-- containers
import Data.Map.Lazy (Map, filter)

-- hasql
import Hasql.Session (QueryError)

-- transformers
import Control.Monad.Trans.Except (ExceptT)

inMemoryContentRepository :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> ContentRepository (ExceptT QueryError IO)
inMemoryContentRepository contentsMap = ContentRepository
  { selectUserContentsByTags = inMemorySelectUserContentsByTags contentsMap
  , addContentWithTags       = inMemoryAddContentWithTags contentsMap
  }

inMemorySelectUserContentsByTags :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> Id User -> [Tag] -> ExceptT QueryError IO [Owned (Content Tag)]
inMemorySelectUserContentsByTags contentsMap id' tags = liftIO . atomically $ do
  contents <- readTVar contentsMap
  let userContentsWithTags = filter (\content -> _ && hasAllTags tags content) contents
  _

inMemoryAddContentWithTags :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> Id User -> Content Tag -> ExceptT QueryError IO (Id (Content Tag))
inMemoryAddContentWithTags contentsMap id' content = _
