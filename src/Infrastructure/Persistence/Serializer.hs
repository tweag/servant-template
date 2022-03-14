module Infrastructure.Persistence.Serializer where

import qualified Infrastructure.Persistence.Schema as DB (Content(Content), Tag(Tag), User(User))
import Infrastructure.Persistence.Schema (contentId, contentContent, contentUserId, tagId, tagName, userId, userName, userPassword)
import Tagger.Content (Content(..))
import Tagger.Id (Id)
import qualified Tagger.Owned as O (_content, _userId)
import Tagger.Owned (Owned(Owned))
import Tagger.Tag (Tag(Tag))
import qualified Tagger.Tag as T (_name)
import Tagger.User (User(User))
import qualified Tagger.User as U (_name, _password)

-- rel8
import Rel8 (Result)

-- CONTENT

-- |
-- Transform from a domain representation of a 'Content' to its underlying database representation
serializeContent :: Id (Content Tag) -> Id User -> Content (Id Tag, Tag) -> (DB.Content Result, [DB.Tag Result])
serializeContent contentId' userId' content = (dbContent, dbTags)
  where
    dbContent = DB.Content
      { contentId      = contentId'
      , contentContent = _content content
      , contentUserId  = userId'
      }
    dbTags = uncurry serializeTag <$> _tags content

-- |
-- Transform from the database representation of a 'Content' to its domain representation
unserializeContent :: DB.Content Result -> [DB.Tag Result] -> DB.User Result -> Owned (Content Tag)
unserializeContent content tags user = Owned
  { O._content = Content
    { _content = contentContent content
    , _tags    = unserializeTag <$> tags
    }
  , O._userId = userId user
  }

-- TAG

-- |
-- Transform from a domain representation of a 'Tag' to its underlying database representation
serializeTag :: Id Tag -> Tag -> DB.Tag Result
serializeTag uuid tag = DB.Tag
  { tagId   = uuid
  , tagName = T._name tag
  }

-- |
-- Transform from the database representation of a 'Tag' to its domain representation
unserializeTag :: DB.Tag Result -> Tag
unserializeTag tag = Tag (tagName tag)

-- USER

-- |
-- Transform from a domain representation of a 'User' to its underlying database representation
serializeUser :: Id User -> User -> DB.User Result
serializeUser uuid user = DB.User
  { userId   = uuid
  , userName = U._name user
  , userPassword = U._password user
  }

-- |
-- Transform from the database representation of a 'User' to its domain representation
unserializeUser :: DB.User Result -> User
unserializeUser user = User (userName user) (userPassword user)
