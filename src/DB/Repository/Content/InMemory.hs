module DB.Repository.Content.InMemory (Table, repository) where

import AppM (AppM, AppM')
import Control.Monad.IO.Class (liftIO)
import Data.Map.Lazy (Map, elems, filter, insert)
import Data.UUID.V4 (nextRandom)
import GHC.Conc (TVar, atomically, readTVar, writeTVar)
import Tagger.Content (Content, hasAllTags)
import Tagger.Id (Id (Id))
import Tagger.Owned (Owned (..))
import Tagger.Repository.Content (ContentRepository (..))
import Tagger.Tag (Tag)
import Tagger.User (User)
import Prelude hiding (filter)

type Table = TVar (Map (Id (Content Tag)) (Owned (Content Tag)))

repository :: Table -> ContentRepository AppM'
repository contentsMap =
  ContentRepository
    { selectUserContentsByTags = inMemorySelectUserContentsByTags contentsMap,
      addContentWithTags = inMemoryAddContentWithTags contentsMap
    }

inMemorySelectUserContentsByTags :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> Id User -> [Tag] -> AppM [Owned (Content Tag)]
inMemorySelectUserContentsByTags contentsMap userId' tags = liftIO . atomically $ do
  contents <- readTVar contentsMap
  let userContentsWithTags = filter ((&&) <$> ((== userId') . userId) <*> (hasAllTags tags . content)) contents
  pure $ elems userContentsWithTags

inMemoryAddContentWithTags :: TVar (Map (Id (Content Tag)) (Owned (Content Tag))) -> Id User -> Content Tag -> AppM (Id (Content Tag))
inMemoryAddContentWithTags contentsMap userId' content' = do
  contentId <- Id <$> liftIO nextRandom
  liftIO . atomically $ do
    contents <- readTVar contentsMap
    writeTVar contentsMap $ insert contentId (Owned userId' content') contents
  pure contentId
