module Infrastructure.Persistence.Queries where

import Infrastructure.Persistence.Schema (Content(..), contentSchema, Tag(..), tagSchema, ContentsTags(..), contentsTagsSchema)

import Prelude hiding (filter)

-- hasql
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)

-- hasql-transaction
import qualified Hasql.Transaction as T (statement)
import Hasql.Transaction.Sessions (transaction, IsolationLevel (Serializable), Mode (Write))

-- rel8
import Rel8 (Expr, Insert(..), OnConflict(..), Query, Result, each, filter, insert, many, select, values, where_, (==.), TableSchema, Name, Rel8able)


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

-- ADD CONTENT WITH TAGS

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

addContentWithTags :: Content Expr -> [Tag Expr] -> Session ()
addContentWithTags content tags = transaction Serializable Write $ do
  T.statement () (add contentSchema [content])
  T.statement () (add tagSchema tags)
  T.statement () (add contentsTagsSchema $ contentTag content <$> tags)
