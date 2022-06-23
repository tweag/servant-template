module UI.Form.Content where

import UI

newContent :: Monad m => HtmlT m ()
newContent = do
  -- TODO This is hardcoding empty tags
  form_ [hxPost_ "/contents", hxVals_ "{ \"tags\": [] }"] $ do
    div_ [] $ do
      let inputName = "input-message"
      label_ [for_ inputName] "New Content"
      input_
        [ id_ inputName,
          name_ "message",
          placeholder_ "New Content",
          type_ "text"
        ]
    div_ [] $ do
      let inputName = "input-tags"
      label_ [for_ inputName] "tags"
      input_
        [ id_ inputName,
          name_ "tags",
          placeholder_ "tag1, tag2",
          type_ "text"
        ]

    button_ [type_ "submit"] "Submit"
