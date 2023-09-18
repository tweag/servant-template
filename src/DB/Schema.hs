module DB.Schema where

import GHC.Generics (Generic)
import Rel8 (Column, Name, Rel8able, TableSchema (..))
import Tagger.Content qualified as Domain (Content)
import Tagger.Id (Id)
import Tagger.Tag qualified as Domain (Tag)

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
