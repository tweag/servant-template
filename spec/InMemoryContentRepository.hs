module InMemoryContentRepository where

import Tagger.Content (Content, hasAllTags)
import Tagger.ContentRepository (ContentRepository(..))
import Tagger.Id (Id(Id))
import Tagger.Owned (Owned(..))
import Tagger.Tag (Tag)
import Tagger.User (User)

-- base
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (TVar, atomically, readTVar, writeTVar)
import Prelude hiding (filter)

-- containers
import Data.Map.Lazy (Map, elems, filter, insert)

-- hasql
import Hasql.Session (QueryError)

-- transformers
import Control.Monad.Trans.Except (ExceptT)

-- uuid
import Data.UUID.V4 (nextRandom)

inMemoryContentRepository :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> ContentRepository (ExceptT QueryError IO)
inMemoryContentRepository contentsMap = ContentRepository
  { selectUserContentsByTags = inMemorySelectUserContentsByTags contentsMap
  , addContentWithTags       = inMemoryAddContentWithTags contentsMap
  }

inMemorySelectUserContentsByTags :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> Id User -> [Tag] -> ExceptT QueryError IO [Owned (Content Tag)]
inMemorySelectUserContentsByTags contentsMap userId' tags = liftIO . atomically $ do
  contents <- readTVar contentsMap
  let userContentsWithTags = filter ((&&) <$> ((== userId') . userId) <*> (hasAllTags tags . content)) contents
  pure $ elems userContentsWithTags

inMemoryAddContentWithTags :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> Id User -> Content Tag -> ExceptT QueryError IO (Id (Content Tag))
inMemoryAddContentWithTags contentsMap userId' content' = do
  contentId <- Id <$> liftIO nextRandom
  liftIO . atomically $ do
    contents <- readTVar contentsMap
    writeTVar contentsMap $ insert contentId (Owned userId' content') contents
  pure contentId
