{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Infrastructure.Persistence.Queries where

import qualified Data.List as List (filter)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction (statement)
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Write), transaction)
import Infrastructure.Persistence.Schema (Content (..), ContentsTags (..), Tag (..), User (userName), contentSchema, contentsTagsSchema, litContent, litTag, tagSchema, userId, userSchema)
import Rel8 (Expr, Insert (..), Name, OnConflict (..), Query, Rel8able, Result, TableSchema, each, filter, in_, insert, lit, many, select, values, where_, (==.))
import Tagger.Id (Id)
import qualified Tagger.User as Domain (User)
import Prelude hiding (filter)

-- SELECT CONTENTS

-- |
-- Selects the 'ContentsTags' for a given 'Content'
contentsTagsForContent :: Content Expr -> Query (ContentsTags Expr)
contentsTagsForContent content =
  each contentsTagsSchema
    >>= filter
      ( \contentTag' ->
          ctContentId contentTag' ==. contentId content
      )

-- |
-- Selects the 'Tags' associated with a given 'Content'
tagsForContent :: Content Expr -> Query (Tag Expr)
tagsForContent content = do
  tag <- each tagSchema
  contentTag' <- contentsTagsForContent content
  where_ $ tagId tag ==. ctTagId contentTag'
  return tag

-- |
-- Selects the 'User' who ownes a 'Content'
userForContent :: Content Expr -> Query (User Expr)
userForContent content =
  each userSchema
    >>= filter
      ( \user ->
          userId user ==. contentUserId content
      )

-- |
-- Given a 'Domain.User' 'Id', retrieves all the contents for that specific user
selectUserContents :: Id Domain.User -> Session [(Content Result, [Tag Result], User Result)]
selectUserContents userId' = statement () . select $ do
  -- Select all content for the given user
  content <-
    each contentSchema
      >>= filter
        ( \content ->
            contentUserId content ==. lit userId'
        )
  -- Select tags for each content
  tags <- many $ tagsForContent content
  -- Select user for each content
  user <- userForContent content
  return (content, tags, user)

-- SELECT TAGS

-- |
-- Selects all tags present in the database among the requested ones
selectTags :: [Tag Result] -> Statement () [Tag Result]
selectTags tagNames = select $ each tagSchema >>= filter ((`in_` (tagName . litTag <$> tagNames)) . tagName)

-- ADD CONTENT

-- |
-- Adds a number of rows to the specified 'TableSchema'
add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Statement () ()
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
contentTag :: Content f -> Tag f -> ContentsTags f
contentTag content tag =
  ContentsTags
    { ctContentId = contentId content,
      ctTagId = tagId tag
    }

-- |
-- Removes the 'alreadyPresentTags' from 'allTags'
removeAlreadyPresentTags :: [Tag Result] -> [Tag Result] -> [Tag Result]
removeAlreadyPresentTags allTags alreadyPresentTags = List.filter (\tag -> tagName tag `notElem` (tagName <$> alreadyPresentTags)) allTags

-- |
-- Given a 'Content' and a list of 'Tag's, it inserts the new content into the database associating to it the provided tags.
-- To avoid 'Tag' repetitions, it goes through the following steps:
--
-- * selects 'Tag's from the database
-- * replaces the generated 'UUID's with the one coming from the database
-- * inserts the new 'Tag's
-- * inserts the 'Content'
-- * inserts the 'ContentsTags' to link the 'Content' with its 'Tags'
addContentWithTags :: Content Result -> [Tag Result] -> Session ()
addContentWithTags content tags = transaction Serializable Write $ do
  alreadyPresentTags <- Transaction.statement () (selectTags tags)
  let newTags = litTag <$> removeAlreadyPresentTags tags alreadyPresentTags
  Transaction.statement () $ add tagSchema newTags
  Transaction.statement () $ add contentSchema [litContent content]
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
selectUserByName :: Text -> Session (Either WrongNumberOfResults (User Result))
selectUserByName name = statement () query
  where
    query = fmap justOne . select $ do
      users <- each userSchema
      filter (\user -> userName user ==. lit name) users

-- ADD USER

-- |
-- Add a new 'User' in the database
addUser :: User Expr -> Session ()
addUser = statement () . add userSchema . pure
