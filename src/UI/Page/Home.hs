module UI.Page.Home where

import Data.Text (Text)
import Html
import UI

data View = View

instance ToHtml View where
  toHtmlRaw = toHtml
  toHtml _ = toHtml view

view :: Html ()
view =
  mainLayout $ do
    h1_ "Tagger"
    UI.div [id_ "flash"] ""
    signupForm
    loginForm

signupForm :: Html ()
signupForm = do
  accountForm "Register User" "register"

loginForm :: Html ()
loginForm = do
  accountForm "Login" "login"

accountForm :: Text -> Text -> Html ()
accountForm label routeName = do
  h2_ $ toHtml label

  UI.form
    [ hxPost_ $ "/" <> routeName,
      hxTarget_ "#flash",
      hxSwap OuterHtml []
    ]
    $ do
      UI.div [] $ do
        let inputName = routeName <> "-username"
        label_ [for_ inputName] "Username"
        input_
          [ id_ inputName,
            name_ "username",
            placeholder_ "Username",
            type_ "username"
          ]

      UI.div [] $ do
        let inputName = routeName <> "-password"
        label_ [for_ inputName] "Password"
        input_
          [ id_ inputName,
            name_ "password",
            placeholder_ "Password",
            type_ "password"
          ]

      UI.button [type_ "submit"] "Submit"
