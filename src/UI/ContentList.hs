module UI.ContentList where

import Control.Monad (forM_)
import Tagger.Content (Content)
import qualified Tagger.Content as Content
import Tagger.Owned (Owned)
import qualified Tagger.Owned as Owned (content)
import Tagger.Tag (Tag)
import qualified Tagger.Tag as Tag
import UI

view :: [Owned (Content Tag)] -> Html ()
view contents = do
  div_ [id_ "contents", class_ "w-full"] $ do
    searchBar
    table_ [id_ "contents", class_ "w-full table-fixed border-separate border-spacing-2"] $
      case contents of
        [] -> "No content"
        _ -> do
          thead_ $ do
            th_ "Content"
            th_ "Tags"
          tbody_ $
            forM_ contents $ \ownedContent ->
              tr_ $ do
                let content = Owned.content ownedContent
                    message = Content.message content
                    tTags = Tag.name <$> Content.tags content
                    rowCell = td_ [class_ "border border-slate-100"]
                rowCell (toHtml message)
                rowCell $ forM_ tTags (p_ . toHtml)

searchBar :: Html ()
searchBar =
  textInput $
    InputProps
      { inputName = "tag",
        inputLabel = "Search",
        inputType = Just "text",
        inputPlaceholder = Just "Filter by tags",
        extraAttrs =
          [ hxGet_ "/contents",
            hxTarget "contents",
            hxParams_ "tag",
            hxTrigger $ BrowserEvt KeyUp [Delay (Milliseconds 500)]
          ]
      }
