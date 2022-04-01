{-# LANGUAGE TupleSections #-}

module Infrastructure.Persistence.PostgresContentRepository where

import Infrastructure.Persistence.Serializer (unserializeContent, serializeContent)
import qualified Infrastructure.Persistence.Queries as DB (selectUserContents, addContentWithTags)
import Tagger.Content (Content, hasAllTags)
import Tagger.ContentRepository (ContentRepository(..))
import Tagger.Id (Id(Id))
import Tagger.Owned (Owned(content))
import Tagger.Tag (Tag)
import Tagger.User (User)

-- base
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)

-- extra
import Data.Tuple.Extra (uncurry3)

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (run, QueryError)

-- transformers
import Control.Monad.Trans.Except (ExceptT (ExceptT))

-- uuid
import Data.UUID.V4 (nextRandom)

-- |
-- A 'ContentRepository' based on PostgreSQL
postgresContentRepository :: Connection -> ContentRepository (ExceptT QueryError IO)
postgresContentRepository connection = ContentRepository
  { selectUserContentsByTags = postgresSelectUserContentsByTags connection
  , addContentWithTags       = postgresAddContentWithTags connection
  }

postgresSelectUserContentsByTags :: Connection -> Id User -> [Tag] -> ExceptT QueryError IO [Owned (Content Tag)]
postgresSelectUserContentsByTags connection userId tags = do
  -- Retrieve the user's contents data from the database
  userDBContents <- ExceptT $ run (DB.selectUserContents userId) connection
  -- Convert the contents data into their domain representation
  let userContents = uncurry3 unserializeContent <$> userDBContents
  -- Filter only the contents indexed by the provided tags
  pure $ filter (hasAllTags tags . content) userContents

postgresAddContentWithTags :: Connection -> Id User -> Content Tag -> ExceptT QueryError IO (Id (Content Tag))
postgresAddContentWithTags connection userId content' = do
  -- Generate a UUID for the content
  contentUUID          <- liftIO nextRandom
  -- Generate and associate a UUID to every tag
  contentWithTagsUUIDs <- liftIO $ forM content' (\tag -> (, tag) . Id <$> nextRandom)
  -- Run a transaction to add the content and its tags to the database
  ExceptT $ run (uncurry DB.addContentWithTags $ serializeContent (Id contentUUID) userId contentWithTagsUUIDs) connection
  pure $ Id contentUUID
