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
  h2_ $ toHtml label

  form_
    [hxPost_ $ "/" <> routeName]
    $ do
      div_ [] $ do
        let inputName = routeName <> "-username"
        label_ [for_ inputName] "Username"
        input_
          [ id_ inputName,
            name_ "username",
            placeholder_ "Username",
            type_ "username"
          ]

      div_ [] $ do
        let inputName = routeName <> "-password"
        label_ [for_ inputName] "Password"
        input_
          [ id_ inputName,
            name_ "password",
            placeholder_ "Password",
            type_ "password"
          ]

      button_ [type_ "submit"] "Submit"
