{-# LANGUAGE TupleSections #-}

module Infrastructure.Persistence.PostgresContentRepository where

import Infrastructure.Persistence.Serializer (unserializeContent, serializeContent)
import qualified Infrastructure.Persistence.Queries as DB (selectUserContents, addContentWithTags)
import Tagger.Content (Content, hasAllTags)
import Tagger.ContentRepository (ContentRepository(..))
import Tagger.Id (Id(Id))
import Tagger.Owned (Owned)
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

postgresContentRepository :: Connection -> ContentRepository (ExceptT QueryError IO)
postgresContentRepository connection = ContentRepository
  { selectUserContentsByTags = postgresSelectUserContentsByTags connection
  , addContentWithTags       = postgresAddContentWithTags connection
  }

-- TODO: filter the contents on the db side
postgresSelectUserContentsByTags :: Connection -> Id User -> [Tag] -> ExceptT QueryError IO [Owned (Content Tag)]
postgresSelectUserContentsByTags connection userId tags = do
  userDBContents <- ExceptT $ run (DB.selectUserContents userId) connection
  let userContents = uncurry3 unserializeContent <$> userDBContents
  pure $ filter (hasAllTags tags) userContents

-- steps:
--  - generate UUID for content
--  - generate UUIDs for tags
--  - transaction
--    - select tags from db
--    - replace generated UUID with the one coming from the database
--    - insert new tags
--    - insert content
--    - insert contents_tags
postgresAddContentWithTags :: Connection -> Id User -> Content Tag -> ExceptT QueryError IO (Id (Content Tag))
postgresAddContentWithTags connection userId content = do
  contentUUID          <- liftIO nextRandom
  contentWithTagsUUIDs <- liftIO $ forM content (\tag -> (, tag) . Id <$> nextRandom)
  ExceptT $ run (uncurry DB.addContentWithTags $ serializeContent (Id contentUUID) userId contentWithTagsUUIDs) connection
  pure $ Id contentUUID
