{-# LANGUAGE TupleSections #-}

module Infrastructure.Persistence.PostgresContentRepository where

import Infrastructure.Persistence.Serializer (unserializeContent, serializeContent)
import qualified Infrastructure.Persistence.Queries as DB (selectAllContentsWithTags, addContentWithTags)
import Tagger.Content (Content, hasAllTags)
import Tagger.ContentRepository (ContentRepository(..))
import Tagger.Id (Id(Id))
import Tagger.Tag (Tag)

-- base
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (run, QueryError)

-- transformers
import Control.Monad.Trans.Except (ExceptT (ExceptT))

-- uuid
import Data.UUID.V4 (nextRandom)

-- TODO:: should I use Reader to keep track of the Connection?
postgresContentRepository :: Connection -> ContentRepository (ExceptT QueryError IO)
postgresContentRepository connection = ContentRepository
  { selectContentsByTags = postgresSelectContentsByTags connection
  , addContentWithTags   = postgresAddContentWithTags connection
  }

-- TODO: filter the contents on the db side
postgresSelectContentsByTags :: Connection -> [Tag] -> ExceptT QueryError IO [Content Tag]
postgresSelectContentsByTags connection tags = do
  allDBContents <- ExceptT $ run DB.selectAllContentsWithTags connection
  let allContents = uncurry unserializeContent <$> allDBContents
  pure $ filter (hasAllTags tags) allContents

-- steps:
--  - generate UUID for content
--  - generate UUIDs for tags
--  - transaction
--    - select tags from db
--    - replace generated UUID with the one coming from the database
--    - insert new tags
--    - insert content
--    - insert contents_tags
postgresAddContentWithTags :: Connection -> Content Tag -> ExceptT QueryError IO (Id (Content Tag))
postgresAddContentWithTags connection content = do
  contentUUID          <- liftIO nextRandom
  contentWithTagsUUIDs <- liftIO $ forM content (\tag -> (, tag) . Id <$> nextRandom)
  ExceptT $ run (uncurry DB.addContentWithTags $ serializeContent (Id contentUUID) contentWithTagsUUIDs) connection
  pure $ Id contentUUID
