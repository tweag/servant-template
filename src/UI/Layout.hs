module UI.Layout (Anchor (..), mainLayout, anchors) where

import qualified Data.Text as T
import Html
import qualified Temp

mainLayout :: Html () -> Html ()
mainLayout body' = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Home - [Tagger]"

      loadScript "https://unpkg.com/htmx.org@1.7.0"
      loadScript "https://unpkg.com/htmx.org/dist/ext/json-enc.js"
      loadScript "https://cdn.tailwindcss.com"

    body_ [extensions, class_ "h-full"] $ do
      div_ [id_ $ headersAnchor anchors, headers] $ do
        section_ [id_ $ flashAnchor anchors] ""

        section_ [id_ $ mainContentAnchor anchors, class_ "h-full"] $
          toHtml body'

        sessionScript

data Anchor = Anchor
  { flashAnchor :: T.Text,
    mainContentAnchor :: T.Text,
    headersAnchor :: T.Text
  }

anchors :: Anchor
anchors =
  Anchor
    { flashAnchor = "flash",
      mainContentAnchor = "mainContent",
      headersAnchor = "headersAnchor"
    }

headers :: Attribute
headers = hxHeaders [("Accept", "text/html")]

extensions :: Attribute
extensions = hxExt "json-enc"

loadScript :: T.Text -> Html ()
loadScript url = with (term "script" [src_ url, defer_ "true"]) [] ""

sessionScript :: Html ()
sessionScript =
  script_ $
    " document.body.addEventListener('" <> Temp.loggedInEvt <> "', function(event) { \
      \ let token = event.detail.value; \
      \ let headers = JSON.parse(document.getElementById('" <> headersAnchor anchors <> "').dataset.hxHeaders); \
      \ headers['Authorization'] = \"Bearer \" + token;\
      \ document.getElementById('" <> headersAnchor anchors <> "').dataset.hxHeaders = JSON.stringify(headers); \
    \;});"
