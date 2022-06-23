module UI.ContentList where

import Control.Monad (forM_)
import Tagger.Content (Content)
import qualified Tagger.Content as Content
import Tagger.Owned (Owned)
import qualified Tagger.Owned as Owned (content)
import Tagger.Tag (Tag)
import qualified Tagger.Tag as Tag
import UI

view :: Monad m => [Owned (Content Tag)] -> HtmlT m ()
view contents =
  table_ [id_ "contents"] $ do
    th_ "Content"
    th_ "Tags"
    forM_ contents $ \ownedContent ->
      tr_ $ do
        let content = Owned.content ownedContent
            message = Content.message content
            tTags = Tag.name <$> Content.tags content
        td_ (toHtml message)
        td_ $
          forM_ tTags (p_ . toHtml)
