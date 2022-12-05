{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Infrastructure.Persistence.Schema where

import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (Column, Expr, Name, Rel8able, Result, TableSchema (..), lit)
import qualified Tagger.Content as Domain (Content)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id)
import qualified Tagger.Tag as Domain (Tag)
import qualified Tagger.User as Domain (User)

-- TAG

-- |
-- The database representation of a 'Tag'
data Tag f = Tag
  { tagId :: Column f (Id Domain.Tag),
    tagName :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'Tag' table
tagSchema :: TableSchema (Tag Name)
tagSchema =
  TableSchema
    { name = "tags",
      schema = Nothing,
      columns =
        Tag
          { tagId = "id",
            tagName = "name"
          }
    }

-- |
-- Allows to lift a 'Tag' with no context into the 'Expr' context
litTag :: Tag Result -> Tag Expr
litTag (Tag id' name') = Tag (lit id') (lit name')

-- CONTENT

-- |
-- The database representation of a 'Content'
data Content f = Content
  { contentId :: Column f (Id (Domain.Content Domain.Tag)),
    contentContent :: Column f Text,
    contentUserId :: Column f (Id Domain.User)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'Content' table
contentSchema :: TableSchema (Content Name)
contentSchema =
  TableSchema
    { name = "contents",
      schema = Nothing,
      columns =
        Content
          { contentId = "id",
            contentContent = "content",
            contentUserId = "user_id"
          }
    }

-- |
-- Allows to lift a 'Content' with no context into the 'Expr' context
litContent :: Content Result -> Content Expr
litContent (Content id' content' userId') = Content (lit id') (lit content') (lit userId')

-- CONTENTS_TAGS

-- |
-- The database representation of a connection between a 'Content' and a 'Tag'
data ContentsTags f = ContentsTags
  { ctContentId :: Column f (Id (Domain.Content Domain.Tag)),
    ctTagId :: Column f (Id Domain.Tag)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'ContentsTags' table
contentsTagsSchema :: TableSchema (ContentsTags Name)
contentsTagsSchema =
  TableSchema
    { name = "contents_tags",
      schema = Nothing,
      columns =
        ContentsTags
          { ctContentId = "content_id",
            ctTagId = "tag_id"
          }
    }

-- USERS

-- |
-- The database representation of a 'User'
data User f = User
  { userId :: Column f (Id Domain.User),
    userName :: Column f Text,
    userPassword :: Column f EncryptedPassword
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'User' table
userSchema :: TableSchema (User Name)
userSchema =
  TableSchema
    { name = "users",
      schema = Nothing,
      columns =
        User
          { userId = "id",
            userName = "name",
            userPassword = "password"
          }
    }

-- |
-- Allows to lift a 'User' with no context into the 'Expr' context
litUser :: User Result -> User Expr
litUser (User id' name' password) = User (lit id') (lit name') (lit password)
