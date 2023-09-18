module DB.Schema.Content
  ( Row (..),
    relation,
    litContent,
    serializeContent,
    unserializeContent,
  )
where

import DB.Schema.Tag (serializeTag, unserializeTag)
import DB.Schema.Tag qualified as Tag
import DB.Schema.User qualified as User
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8
import Tagger.Content (Content (..), createContent)
import Tagger.Id (Id)
import Tagger.Owned (Owned (Owned))
import Tagger.Owned qualified as Owned (content, userId)
import Tagger.Tag (Tag)
import Tagger.User (User)

-- |
-- The database representation of a 'Content'
data Row f = Row
  { contentId :: Column f (Id (Content Tag)),
    contentContent :: Column f Text,
    contentUserId :: Column f (Id User)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'Content' table
relation :: TableSchema (Row Name)
relation =
  TableSchema
    { name = "contents",
      schema = Nothing,
      columns =
        Row
          { contentId = "id",
            contentContent = "content",
            contentUserId = "user_id"
          }
    }

-- |
-- Allows to lift a 'Content' with no context into the 'Expr' context
litContent :: Row Result -> Row Expr
litContent (Row id' content' userId') = Row (lit id') (lit content') (lit userId')

-- |
-- Transform from a domain representation of a 'Content' to its underlying database representation
serializeContent :: Id (Content Tag) -> Id User -> Content (Id Tag, Tag) -> (Row Result, [Tag.Row Result])
serializeContent contentId' userId' content = (dbContent, dbTags)
  where
    dbContent =
      Row
        { contentId = contentId',
          contentContent = message content,
          contentUserId = userId'
        }
    dbTags = uncurry serializeTag <$> tags content

-- |
-- Transform from the database representation of a 'Content' to its domain representation
unserializeContent :: Row Result -> [Tag.Row Result] -> User.Row Result -> Owned (Content Tag)
unserializeContent content tags' user =
  Owned
    { Owned.content =
        createContent
          (contentContent content)
          (unserializeTag <$> tags'),
      Owned.userId = user.userId
    }
