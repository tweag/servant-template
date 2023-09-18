module DB.Repository.Content.Postgres (repository) where

import AppM (AppM, AppM')
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import DB.Queries qualified as DB (addContentWithTags, selectUserContents)
import DB.Schema.Content (serializeContent, unserializeContent)
import Data.Tuple.Extra (uncurry3)
import Data.UUID.V4 (nextRandom)
import Tagger.Content (Content, hasAllTags)
import Tagger.Database (runQuery)
import Tagger.Id (Id (Id))
import Tagger.Owned (Owned (content))
import Tagger.Repository.Content (ContentRepository (..))
import Tagger.Tag (Tag)
import Tagger.User (User)

-- |
-- A 'ContentRepository' based on PostgreSQL
repository :: ContentRepository AppM'
repository =
  ContentRepository
    { selectUserContentsByTags = postgresSelectUserContentsByTags,
      addContentWithTags = postgresAddContentWithTags
    }

postgresSelectUserContentsByTags :: Id User -> [Tag] -> AppM [Owned (Content Tag)]
postgresSelectUserContentsByTags userId tags = do
  -- Retrieve the user's contents data from the database
  userDBContents <- runQuery (DB.selectUserContents userId)
  -- Convert the contents data into their domain representation
  let userContents = uncurry3 unserializeContent <$> userDBContents
  -- Filter only the contents indexed by the provided tags
  pure $ filter (hasAllTags tags . content) userContents

postgresAddContentWithTags :: Id User -> Content Tag -> AppM (Id (Content Tag))
postgresAddContentWithTags userId content' = do
  -- Generate a UUID for the content
  contentUUID <- liftIO nextRandom
  -- Generate and associate a UUID to every tag
  contentWithTagsUUIDs <- liftIO $ forM content' (\tag -> (,tag) . Id <$> nextRandom)
  -- Run a transaction to add the content and its tags to the database
  runQuery (uncurry DB.addContentWithTags $ serializeContent (Id contentUUID) userId contentWithTagsUUIDs)
  pure $ Id contentUUID
