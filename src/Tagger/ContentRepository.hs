{-# LANGUAGE RankNTypes #-}

module Tagger.ContentRepository where

import Tagger.Content (Content)
import Tagger.Id (Id)
import Tagger.Tag (Tag)
import Tagger.Owned (Owned)
import Tagger.User (User)

data ContentRepository m = ContentRepository
  { selectUserContentsByTags :: Id User -> [Tag] -> m [Owned (Content Tag)]
  , addContentWithTags       :: Id User -> Content Tag -> m (Id (Content Tag))
  }

hoistContentRepository :: (forall a. m a -> n a) -> ContentRepository m -> ContentRepository n
hoistContentRepository f (ContentRepository select add) = ContentRepository ((f .) . select) ((f .) . add)
