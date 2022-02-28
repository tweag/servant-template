{-# LANGUAGE RankNTypes #-}

module Tagger.ContentRepository where

import Tagger.Content (Content)
import Tagger.Tag (Tag)

-- uuid
import Data.UUID (UUID)

data ContentRepository m = ContentRepository
  { selectContentsByTags :: [Tag] -> m [Content Tag]
  , addContentWithTags   :: Content Tag -> m UUID
  }

hoist :: (forall a. m a -> n a) -> ContentRepository m -> ContentRepository n
hoist f (ContentRepository select add) = ContentRepository (f . select) (f . add)