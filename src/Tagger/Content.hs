module Tagger.Content where

import Tagger.Tag (Tag)

-- text
import Data.Text (Text)

data Content = Content
  { _content :: Text
  , _tags :: [Tag]
  }
