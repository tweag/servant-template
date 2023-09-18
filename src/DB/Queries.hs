module DB.Queries where

import DB.Schema (ContentsTags (..), contentsTagsSchema)
import DB.Schema.Content (litContent)
import DB.Schema.Content qualified as Content
import DB.Schema.Tag (litTag)
import DB.Schema.Tag qualified as Tag
import DB.Schema.User qualified as User
import Data.List qualified as List (filter)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import Hasql.Transaction qualified as Transaction (statement)
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Write), transaction)
import Rel8 (Expr, Insert (..), Name, OnConflict (..), Query, Rel8able, Result, TableSchema, each, filter, in_, insert, lit, many, select, values, where_, (==.))
import Tagger.Id (Id)
import Tagger.User qualified as Domain (User)
import Prelude hiding (filter)

-- SELECT CONTENTS

-- |
-- Selects the 'ContentsTags' for a given 'Content'
contentsTagsForContent :: Content.Row Expr -> Query (ContentsTags Expr)
contentsTagsForContent content =
  each contentsTagsSchema
    >>= filter
      ( \contentTag' ->
          ctContentId contentTag' ==. content.contentId
      )

-- |
-- Selects the 'Tags' associated with a given 'Content'
tagsForContent :: Content.Row Expr -> Query (Tag.Row Expr)
tagsForContent content = do
  tag <- each Tag.relation
  contentTag' <- contentsTagsForContent content
  where_ $ tag.tagId ==. ctTagId contentTag'
  return tag

-- |
-- Selects the 'User' who ownes a 'Content'
userForContent :: Content.Row Expr -> Query (User.Row Expr)
userForContent content =
  each User.relation
    >>= filter
      ( \user ->
          user.userId ==. content.contentUserId
      )

-- |
-- Given a 'Domain.User' 'Id', retrieves all the contents for that specific user
selectUserContents :: Id Domain.User -> Session [(Content.Row Result, [Tag.Row Result], User.Row Result)]
selectUserContents userId' = statement () . select $ do
  -- Select all content for the given user
  content <-
    each Content.relation
      >>= filter
        ( \content ->
            content.contentUserId ==. lit userId'
        )
  -- Select tags for each content
  tags <- many $ tagsForContent content
  -- Select user for each content
  user <- userForContent content
  return (content, tags, user)

-- SELECT TAGS

-- |
-- Selects all tags present in the database among the requested ones
selectTags :: [Tag.Row Result] -> Statement () [Tag.Row Result]
selectTags tagNames = select $ each Tag.relation >>= filter ((`in_` ((.tagName) . litTag <$> tagNames)) . (.tagName))

-- ADD CONTENT

-- |
-- Adds a number of rows to the specified 'TableSchema'
add :: (Rel8able f) => TableSchema (f Name) -> [f Expr] -> Statement () ()
add schema rows' =
  insert $
    Insert
      { into = schema,
        rows = values rows',
        onConflict = Abort,
        returning = pure ()
      }

-- |
-- Creates a 'ContentTag' given a 'Content' and a 'Tag'
contentTag :: Content.Row f -> Tag.Row f -> ContentsTags f
contentTag content tag =
  ContentsTags
    { ctContentId = content.contentId,
      ctTagId = tag.tagId
    }

-- |
-- Removes the 'alreadyPresentTags' from 'allTags'
removeAlreadyPresentTags :: [Tag.Row Result] -> [Tag.Row Result] -> [Tag.Row Result]
removeAlreadyPresentTags allTags alreadyPresentTags = List.filter (\tag -> tag.tagName `notElem` ((.tagName) <$> alreadyPresentTags)) allTags

-- |
-- Given a 'Content' and a list of 'Tag's, it inserts the new content into the database associating to it the provided tags.
-- To avoid 'Tag' repetitions, it goes through the following steps:
--
-- * selects 'Tag's from the database
-- * replaces the generated 'UUID's with the one coming from the database
-- * inserts the new 'Tag's
-- * inserts the 'Content'
-- * inserts the 'ContentsTags' to link the 'Content' with its 'Tags'
addContentWithTags :: Content.Row Result -> [Tag.Row Result] -> Session ()
addContentWithTags content tags = transaction Serializable Write $ do
  alreadyPresentTags <- Transaction.statement () (selectTags tags)
  let newTags = litTag <$> removeAlreadyPresentTags tags alreadyPresentTags
  Transaction.statement () $ add Tag.relation newTags
  Transaction.statement () $ add Content.relation [litContent content]
  Transaction.statement () $ add contentsTagsSchema (contentTag (litContent content) <$> (litTag <$> alreadyPresentTags) <> newTags)

-- SELECT USER BY USERNAME

-- |
-- Describes the possible error cases for queries that expect exactly one row as a result.
data WrongNumberOfResults
  = NoResults
  | MoreThanOneResult
  deriving (Show)

-- |
-- Given a list of results, succeed if there is only one in the list, otherwise fail with the appropriate error message
justOne :: [a Result] -> Either WrongNumberOfResults (a Result)
justOne = \case
  [] -> Left NoResults
  [a] -> Right a
  _ -> Left MoreThanOneResult

-- |
-- Retrieve from the database a user with the provided name.
-- If in the database we find none or more the one, it returns the appropriate error message
selectUserByName :: Text -> Session (Either WrongNumberOfResults (User.Row Result))
selectUserByName name = statement () query
  where
    query = fmap justOne . select $ do
      users <- each User.relation
      filter (\user -> user.userName ==. lit name) users

-- ADD USER

-- |
-- Add a new 'User' in the database
addUser :: User.Row Expr -> Session ()
addUser = statement () . add User.relation . pure
