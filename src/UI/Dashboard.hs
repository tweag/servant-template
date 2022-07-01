module UI.Dashboard where

import UI
import qualified UI.ContentList
import UI.Form.Content

view :: Html ()
view = do
  h1_ [class_ "mb-2 text-center bg-blue-300 text-xl font-semibold"] "Dashboard"
  div_ [id_ "dashboard", class_ "columns-2 h-full"] $ do
    div_
      [ hxTarget "contents",
        hxSwap OuterHtml [],
        hxTriggers [Load [], hxEvent ContentsUpdated],
        hxGet_ "/contents"
      ]
      (UI.ContentList.view [])
    div_
      [class_ "break-before-column"]
      UI.Form.Content.newContent
