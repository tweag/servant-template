module DB.Schema.Tag
  ( Row (..),
    relation,
    litTag,
    serializeTag,
    unserializeTag,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8
import Tagger.Id (Id)
import Tagger.Tag (Tag (..))

-- |
-- The database representation of a 'Tag'
data Row f = Row
  { tagId :: Column f (Id Tag),
    tagName :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'Tag' table
relation :: TableSchema (Row Name)
relation =
  TableSchema
    { name = "tags",
      schema = Nothing,
      columns =
        Row
          { tagId = "id",
            tagName = "name"
          }
    }

-- |
-- Allows to lift a 'Tag' with no context into the 'Expr' context
litTag :: Row Result -> Row Expr
litTag (Row id' name') = Row (lit id') (lit name')

-- |
-- Transform from a domain representation of a 'Tag' to its underlying database representation
serializeTag :: Id Tag -> Tag -> Row Result
serializeTag uuid tag =
  Row
    { tagId = uuid,
      tagName = tag.name
    }

-- |
-- Transform from the database representation of a 'Tag' to its domain representation
unserializeTag :: Row Result -> Tag
unserializeTag tag = Tag (tagName tag)
