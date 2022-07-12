module UI.Form.Content where

import UI

newContent :: Html ()
newContent =
  div_ [id_ "new-content-form", class_ "columns-2 h-full"] $ do
    messageForm
    addTagForm

messageForm :: Html ()
messageForm =
  form_ [hxTarget "new-content-form", hxPost "/contents", hxInclude "[name='tags']"] $ do
    textInput $
      InputProps
        { inputName = "message",
          inputLabel = "Message",
          inputType = Just "text",
          inputPlaceholder = Just "Content here...",
          extraAttrs = [required_ "true"]
        }

    select_
      [ id_ "content-tags",
        class_ "hidden",
        name_ "tags",
        multiple_ "true"
      ]
      ""

    formButton "Submit"

addTagForm :: Html ()
addTagForm =
  form_
    [ id_ "add-tag-form",
      hxTarget "content-tags",
      hxPost "/add-tag",
      hxSwap BeforeEnd []
    ]
    $ do
      textInput $
        InputProps
          { inputName = "name",
            inputLabel = "Tags",
            inputType = Just "text",
            inputPlaceholder = Just "tag1, tag2",
            extraAttrs = []
          }

      formButton "Add tag"
      div_ [id_ "tag-list"] ""
