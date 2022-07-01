module UI.Form.Account where

import Data.Text (Text)
import UI

registration :: Html ()
registration = do
  div_ [hxTarget This, hxSwap OuterHtml []] $ do
    accountForm "Register User" "register"

login :: Html ()
login = do
  let mainContent = mainContentAnchor anchors
  div_ [hxSwap InnerHtml [], hxTarget (Target mainContent)] $ do
    accountForm "Sign in" "signin"

accountForm :: Text -> Text -> Html ()
accountForm label routeName = do
  div_ [class_ "container"] $ do
    h2_ [class_ "text-center font-medium"] $ toHtml label

    form_ [hxPost_ $ "/" <> routeName] $ do
      textInput $
        InputProps
          { inputName = "username",
            inputLabel = "Username",
            inputType = Just "username",
            inputPlaceholder = Just "johnny_minnesota",
            extraAttrs = []
          }

      textInput $
        InputProps
          { inputName = "password",
            inputLabel = "Password",
            inputType = Just "password",
            inputPlaceholder = Nothing,
            extraAttrs = []
          }

      formButton "Submit"
