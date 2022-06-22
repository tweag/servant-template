module UI.Layout where

import Html

mainLayout :: Html () -> Html ()
mainLayout body' = do
  let headers = hxHeaders_ "{\"Accept\": \"text/html\"}"
      extensions = hxExt_ "json-enc"

  with (html_ [headers, extensions]) [] $ do
    head_ $ do
      title_ "Home - [Tagger]"
      with (term "script" [src_ "https://unpkg.com/htmx.org@1.7.0"]) [] ""
      with (term "script" [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"]) [] ""
    body_ $ toHtml body'
