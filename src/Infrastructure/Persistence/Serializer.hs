module Infrastructure.Persistence.Serializer where

import qualified Infrastructure.Persistence.Schema as DB (Content(Content), Tag(Tag), User(User))
import Infrastructure.Persistence.Schema (contentId, contentContent, tagId, tagName, userId, userName, userPassword)
import Tagger.Content (Content(..))
import Tagger.Id (Id)
import Tagger.Tag (Tag(Tag))
import qualified Tagger.Tag as T (_name)
import Tagger.User (User(User), Password (Password, asBytestring))
import qualified Tagger.User as U (_name, _password)

-- rel8
import Rel8 (Result)

-- CONTENT

serializeContent :: Id (Content Tag) -> Content (Id Tag, Tag) -> (DB.Content Result, [DB.Tag Result])
serializeContent uuid content = (dbContent, dbTags)
  where
    dbContent = DB.Content
      { contentId      = uuid
      , contentContent = _content content
      }
    dbTags = uncurry serializeTag <$> _tags content

unserializeContent :: DB.Content Result -> [DB.Tag Result] -> Content Tag
unserializeContent content tags = Content
  { _content = contentContent content
  , _tags    = unserilizeTag <$> tags
  }

-- TAG

serializeTag :: Id Tag -> Tag -> DB.Tag Result
serializeTag uuid tag = DB.Tag
  { tagId   = uuid
  , tagName = T._name tag
  }

unserilizeTag :: DB.Tag Result -> Tag
unserilizeTag tag = Tag (tagName tag)

-- USER

serializeUser :: Id User -> User -> DB.User Result
serializeUser uuid user = DB.User
  { userId   = uuid
  , userName = U._name user
  , userPassword = asBytestring $ U._password user
  }

unserializeUser :: DB.User Result -> User
unserializeUser user = User (userName user) (Password $ userPassword user)
