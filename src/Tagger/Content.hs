module Tagger.Content where

import Tagger.Tag (Tag)

-- text
import Data.Text (Text)

-- uuid
import Data.UUID (UUID)

data Content = Content
  { _id :: UUID
  , _content :: Text
  , _tags :: [Tag]
  }
