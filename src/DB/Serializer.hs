module DB.Serializer where

import DB.Schema (contentContent, contentId, contentUserId, userId, userName, userPassword)
import DB.Schema qualified as DB
import DB.Schema.Tag (serializeTag, unserializeTag)
import DB.Schema.Tag qualified as Tag
import Rel8 (Result)
import Tagger.Content (Content (..), createContent)
import Tagger.Id (Id)
import Tagger.Owned (Owned (Owned))
import Tagger.Owned qualified as Owned (content, userId)
import Tagger.Tag (Tag)
import Tagger.User (User (User))
import Tagger.User qualified as User (name, password)

-- CONTENT

-- |
-- Transform from a domain representation of a 'Content' to its underlying database representation
serializeContent :: Id (Content Tag) -> Id User -> Content (Id Tag, Tag) -> (DB.Content Result, [Tag.Row Result])
serializeContent contentId' userId' content = (dbContent, dbTags)
  where
    dbContent =
      DB.Content
        { contentId = contentId',
          contentContent = message content,
          contentUserId = userId'
        }
    dbTags = uncurry serializeTag <$> tags content

-- |
-- Transform from the database representation of a 'Content' to its domain representation
unserializeContent :: DB.Content Result -> [Tag.Row Result] -> DB.User Result -> Owned (Content Tag)
unserializeContent content tags' user =
  Owned
    { Owned.content =
        createContent
          (contentContent content)
          (unserializeTag <$> tags'),
      Owned.userId = userId user
    }

-- USER

-- |
-- Transform from a domain representation of a 'User' to its underlying database representation
serializeUser :: Id User -> User -> DB.User Result
serializeUser uuid user =
  DB.User
    { userId = uuid,
      userName = User.name user,
      userPassword = User.password user
    }

-- |
-- Transform from the database representation of a 'User' to its domain representation
unserializeUser :: DB.User Result -> User
unserializeUser user = User (userName user) (userPassword user)
