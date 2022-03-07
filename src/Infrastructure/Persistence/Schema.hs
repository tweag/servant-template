{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Persistence.Schema where

import Tagger.Id (Id)
import qualified Tagger.Content as Domain (Content)
import qualified Tagger.Tag as Domain (Tag)
import qualified Tagger.User as Domain (User)

-- base
import GHC.Generics (Generic)

-- bytestring
import Data.ByteString (ByteString)

-- rel8
import Rel8 (Column, Name, Rel8able, TableSchema(..), Result, Expr, lit)

-- text
import Data.Text (Text)

-- TAG

data Tag f = Tag
  { tagId   :: Column f (Id Domain.Tag)
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

data Content f = Content
  { contentId      :: Column f (Id (Domain.Content Domain.Tag))
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
  { ctContentId :: Column f (Id (Domain.Content Domain.Tag))
  , ctTagId     :: Column f (Id Domain.Tag)
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

data User f = User
  { userId       :: Column f (Id Domain.User)
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
