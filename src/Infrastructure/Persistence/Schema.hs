{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Persistence.Schema where

-- base
import GHC.Generics (Generic)

-- bytestring
import Data.ByteString (ByteString)

-- rel8
import Rel8 (Column, DBEq, DBType, Name, Rel8able, TableSchema(..), Result, Expr, lit)

-- text
import Data.Text (Text)

-- uuid
import Data.UUID (UUID)

-- TAG

newtype TagId = TagId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

data Tag f = Tag
  { tagId   :: Column f TagId
  , tagName :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

tagSchema :: TableSchema (Tag Name)
tagSchema = TableSchema
  { name    = "tags"
  , schema  = Nothing
  , columns = Tag
    { tagId   = "id"
    , tagName = "name"
    }
  }

litTag :: Tag Result -> Tag Expr
litTag (Tag id' name') = Tag (lit id') (lit name')

-- CONTENT

newtype ContentId = ContentId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

data Content f = Content
  { contentId      :: Column f ContentId
  , contentContent :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

contentSchema :: TableSchema (Content Name)
contentSchema = TableSchema
  { name    = "contents"
  , schema  = Nothing
  , columns = Content
    { contentId      = "id"
    , contentContent = "content"
    }
  }

litContent :: Content Result -> Content Expr
litContent (Content id' content') = Content (lit id') (lit content')

-- CONTENTS_TAGS

data ContentsTags f = ContentsTags
  { ctContentId :: Column f ContentId
  , ctTagId     :: Column f TagId
  }
  deriving stock Generic
  deriving anyclass Rel8able

contentsTagsSchema :: TableSchema (ContentsTags Name)
contentsTagsSchema = TableSchema
  { name    = "contents_tags"
  , schema  = Nothing
  , columns = ContentsTags
    { ctContentId = "content_id"
    , ctTagId     = "tag_id"
    }
  }

-- USERS

newtype UserId = UserId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

data User f = User
  { userId       :: Column f UserId
  , userName     :: Column f Text
  , userPassword :: Column f ByteString
  }
  deriving stock Generic
  deriving anyclass Rel8able

userSchema :: TableSchema (User Name)
userSchema = TableSchema
  { name = "users"
  , schema = Nothing
  , columns = User
    { userId = "id"
    , userName = "name"
    , userPassword = "password"
    }
  }

litUser :: User Result -> User Expr
litUser (User id' name' password) = User (lit id') (lit name') (lit password)
