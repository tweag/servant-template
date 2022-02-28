{-# LANGUAGE RankNTypes #-}

module Infrastructure.Persistence.Queries where

import Infrastructure.Persistence.Schema (Content(..), contentSchema, Tag(..), tagSchema, ContentsTags(..), contentsTagsSchema, litTag, litContent)

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
import Rel8 (Expr, Insert(..), OnConflict(..), Query, Result, each, filter, insert, many, select, values, where_, (==.), TableSchema, Name, Rel8able, in_ {-and_, ListTable-})

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

selectTags :: [Tag Result] -> Statement () [Tag Result]
selectTags tagNames = select $ each tagSchema >>= filter ((`in_` (tagName . litTag <$> tagNames)) . tagName)

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

removeAlreadyPresentTags :: [Tag Result] -> [Tag Result] -> [Tag Result]
removeAlreadyPresentTags allTags alreadyPresentTags = List.filter (\tag -> tagName tag `notElem` (tagName <$> alreadyPresentTags)) allTags

-- steps:
--  - select tags from db
--  - replace generated UUID with the one coming from the database
--  - insert new tags
--  - insert content
--  - insert contents_tags
addContentWithTags :: Content Result -> [Tag Result] -> Session ()
addContentWithTags content tags = transaction Serializable Write $ do
  alreadyPresentTags <- T.statement () (selectTags tags)
  let newTags = litTag <$> removeAlreadyPresentTags tags alreadyPresentTags
  T.statement () $ add tagSchema newTags
  T.statement () $ add contentSchema [litContent content]
  T.statement () $ add contentsTagsSchema (contentTag (litContent content) <$> (litTag <$> alreadyPresentTags) <> newTags)