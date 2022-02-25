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

-- TODO: I don't really like the term serialize here
serializeContent :: IO UUID -> Content Tag -> IO (DB.Content Expr, [DB.Tag Expr])
serializeContent uuidGenerator content = do
  uuid <- uuidGenerator
  let dbContent = DB.Content
        { contentId      = lit $ ContentId uuid
        , contentContent = lit $ _content content
        }
  dbTags <- traverse (serializeTag uuidGenerator) (_tags content)
  pure (dbContent, dbTags)

unserializeContent :: DB.Content Result -> [DB.Tag Result] -> Content Tag
unserializeContent content tags = Content
  { _content = contentContent content
  , _tags    = unserilizeTag <$> tags
  }

-- TAG

serializeTag :: IO UUID -> Tag -> IO (DB.Tag Expr)
serializeTag uuidGenerator tag = do
  uuid <- uuidGenerator
  pure DB.Tag
    { tagId   = lit $ TagId uuid
    , tagName = lit $ _name tag
    }

unserilizeTag :: DB.Tag Result -> Tag
unserilizeTag tag = Tag (tagName tag)
