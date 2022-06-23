module UI.Dashboard where

import UI
import qualified UI.ContentList
import UI.Form.Content

view :: Monad m => HtmlT m ()
view = do
  UI.Form.Content.newContent
  div_
    [ hxTarget "contents",
      hxSwap OuterHtml [],
      hxTriggers [ Load [], hxEvent ContentsUpdated],
      hxGet_ "/contents"
    ]
    (UI.ContentList.view [])
