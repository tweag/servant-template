module Infrastructure.Persistence.Serializer where

import qualified Infrastructure.Persistence.Schema as DB (Content(Content), Tag(Tag))
import Infrastructure.Persistence.Schema (contentId, contentContent, tagId, tagName, ContentId (ContentId), TagId (TagId))
import Tagger.Content (Content(..))
import Tagger.Tag (Tag(..))

-- rel8
import Rel8 (Expr, lit, Result)

-- uuid
import Data.UUID (UUID)

-- CONTENT

serializeContent :: UUID -> Content (UUID, Tag) -> (DB.Content Expr, [DB.Tag Expr])
serializeContent uuid content = (dbContent, dbTags)
  where
    dbContent = DB.Content
      { contentId      = lit $ ContentId uuid
      , contentContent = lit $ _content content
      }
    dbTags = uncurry serializeTag <$> _tags content

unserializeContent :: DB.Content Result -> [DB.Tag Result] -> Content Tag
unserializeContent content tags = Content
  { _content = contentContent content
  , _tags    = unserilizeTag <$> tags
  }

-- TAG

serializeTag :: UUID -> Tag -> DB.Tag Expr
serializeTag uuid tag = DB.Tag
  { tagId   = lit $ TagId uuid
  , tagName = lit $ _name tag
  }

unserilizeTag :: DB.Tag Result -> Tag
unserilizeTag tag = Tag (tagName tag)
