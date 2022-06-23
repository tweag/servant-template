module UI.Layout (Anchors (..), mainLayout, anchors) where

import qualified Data.Text as T
import Html

data Anchors = Anchors
  { flashAnchor :: T.Text,
    mainContentAnchor :: T.Text,
    headersAnchor :: T.Text,
    bodyHeadingAnchor :: T.Text
  }

mainLayout :: Html () -> Html () -> Html ()
mainLayout heading body' = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Home - [Tagger]"
      with (term "script" [src_ "https://unpkg.com/htmx.org@1.7.0", defer_ "true"]) [] ""
      with (term "script" [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js", defer_ "true"]) [] ""

    body_ [extensions] $ do
      div_ [id_ $ headersAnchor anchors, headers] $ do
        section_ [id_ $ flashAnchor anchors] ""

        section_ [id_ $ bodyHeadingAnchor anchors]
          heading

        section_ [id_ $ mainContentAnchor anchors] $
          toHtml body'

        sessionScript

anchors :: Anchors
anchors =
  Anchors
    { flashAnchor = "flash",
      mainContentAnchor = "mainContent",
      headersAnchor = "headersAnchor",
      bodyHeadingAnchor = "bodyHeadingAnchor"
    }

headers :: Attribute
headers = hxHeaders_ "{\"Accept\": \"text/html\"}"

extensions :: Attribute
extensions = hxExt_ "json-enc"

sessionScript :: Html ()
sessionScript =
  script_
    "document.body.addEventListener('loggedIn', function(event) {\
      \let token = event.detail.value;\
      \let headers = JSON.parse(document.getElementById('headersAnchor').dataset.hxHeaders);\
      \headers['Authorization'] = \"Bearer \" + token;\
      \document.getElementById('headersAnchor').dataset.hxHeaders = JSON.stringify(headers);\
      \;});"
