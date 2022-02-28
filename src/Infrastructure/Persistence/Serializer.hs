module Infrastructure.Persistence.Serializer where

import qualified Infrastructure.Persistence.Schema as DB (Content(Content), Tag(Tag))
import Infrastructure.Persistence.Schema (contentId, contentContent, tagId, tagName, ContentId (ContentId), TagId (TagId))
import Tagger.Content (Content(..))
import Tagger.Tag (Tag(..))

-- rel8
import Rel8 (Result)

-- uuid
import Data.UUID (UUID)

-- CONTENT

serializeContent :: UUID -> Content (UUID, Tag) -> (DB.Content Result, [DB.Tag Result])
serializeContent uuid content = (dbContent, dbTags)
  where
    dbContent = DB.Content
      { contentId      = ContentId uuid
      , contentContent = _content content
      }
    dbTags = uncurry serializeTag <$> _tags content

unserializeContent :: DB.Content Result -> [DB.Tag Result] -> Content Tag
unserializeContent content tags = Content
  { _content = contentContent content
  , _tags    = unserilizeTag <$> tags
  }

-- TAG

serializeTag :: UUID -> Tag -> DB.Tag Result
serializeTag uuid tag = DB.Tag
  { tagId   = TagId uuid
  , tagName = _name tag
  }

unserilizeTag :: DB.Tag Result -> Tag
unserilizeTag tag = Tag (tagName tag)
