{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

module Infrastructure.Persistence.Schema where

-- base
import GHC.Generics (Generic)

-- rel8
import Rel8 (Column, DBEq, DBType, Name, Rel8able, TableSchema(..))

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
  { name    = "tag"
  , schema  = Nothing
  , columns = Tag
    { tagId   = "id"
    , tagName = "name"
    }
  }

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
  { name    = "content"
  , schema  = Nothing
  , columns = Content
    { contentId      = "id"
    , contentContent = "content"
    }
  }

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
