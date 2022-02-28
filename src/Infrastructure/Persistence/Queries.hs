{-# LANGUAGE RankNTypes #-}

module Infrastructure.Persistence.Queries where

import Infrastructure.Persistence.Schema (Content(..), contentSchema, Tag(..), tagSchema, ContentsTags(..), contentsTagsSchema, ContentId (ContentId))
import qualified Tagger.Content as Domain (Content (_tags, _content))
import qualified Tagger.Tag as Domain (Tag (_name))

-- base
import qualified Data.List as List (filter)
import Prelude hiding (filter)

-- hasql
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)

-- hasql-transaction
import qualified Hasql.Transaction as T (statement)
import Hasql.Transaction.Sessions (transaction, IsolationLevel (Serializable), Mode (Write))

-- rel8
import Rel8 (Expr, Insert(..), OnConflict(..), Query, Result, each, filter, insert, many, select, values, where_, (==.), TableSchema, Name, Rel8able, in_, lit {-and_, ListTable-})

-- text
import Data.Text (Text)

-- uuid
import Data.UUID (UUID)
import Infrastructure.Persistence.Serializer (serializeTag)

-- SELECT CONTENTS WITH TAGS

contentsTagsForContent :: Content Expr -> Query (ContentsTags Expr)
contentsTagsForContent content = each contentsTagsSchema >>= filter (\contentTag' ->
  ctContentId contentTag' ==. contentId content)

tagsForContent :: Content Expr -> Query (Tag Expr)
tagsForContent content = do
  tag         <- each tagSchema
  contentTag' <- contentsTagsForContent content
  where_ $ tagId tag ==. ctTagId contentTag'
  return tag

selectAllContentsWithTags :: Session [(Content Result, [Tag Result])]
selectAllContentsWithTags = statement () . select $ do
  content <- each contentSchema
  tags    <- many $ tagsForContent content
  return (content, tags)

-- selectContentsByTags :: [Tag Expr] -> Session [(Content Result, [Tag Result])]
-- selectContentsByTags tags = statement () . select $ do
--   content <- each contentSchema
--   tags'   <- many $ tagsForContent content
--   filter (\(_, tags'') -> and_ $ isContainedIn tags'' <$> tags) (content, tags')

-- isContainedIn :: ListTable Expr (f Expr) -> f Expr -> Expr Bool
-- isContainedIn table row = _

-- SELECT TAGS

selectTags :: [Text] -> Statement () [Tag Result]
selectTags tagNames = select $ each tagSchema >>= filter ((`in_` (lit <$> tagNames)) . tagName)

-- ADD CONTENT

add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Statement () ()
add schema rows' = insert $ Insert
  { into = schema
  , rows = values rows'
  , onConflict = Abort
  , returning = pure ()
  }

contentTag :: Content Expr -> Tag Expr -> ContentsTags Expr
contentTag content tag = ContentsTags
  { ctContentId = contentId content
  , ctTagId     = tagId     tag
  }

litTag :: Tag Result -> Tag Expr
litTag (Tag id' name') = Tag (lit id') (lit name')

removeAlreadyPresentTags :: [(UUID, Domain.Tag)] -> [Tag Result] -> [(UUID, Domain.Tag)]
removeAlreadyPresentTags allTags alreadyPresentTags = List.filter ((\t -> t `elem` (tagName <$> alreadyPresentTags)) . Domain._name . snd) allTags

-- steps:
--  - select tags from db
--  - replace generated UUID with the one coming from the database
--  - insert new tags
--  - insert content
--  - insert contents_tags
addContentWithTags :: UUID -> Domain.Content (UUID, Domain.Tag) -> Session ()
addContentWithTags contentUUID content = transaction Serializable Write $ do
  let tagsWithUUIDs = Domain._tags content
  alreadyPresentTags <- T.statement () (selectTags $ Domain._name . snd <$> tagsWithUUIDs)
  let newTags = removeAlreadyPresentTags tagsWithUUIDs alreadyPresentTags
  let dbNewTags = uncurry serializeTag <$> newTags
  T.statement () $ add tagSchema dbNewTags
  let contentId' = lit $ ContentId contentUUID
  T.statement () $ add contentSchema [Content contentId' (lit $ Domain._content content)]
  T.statement () $ add contentsTagsSchema (ContentsTags contentId' . tagId <$> (litTag <$> alreadyPresentTags) <> (uncurry serializeTag <$> newTags))
