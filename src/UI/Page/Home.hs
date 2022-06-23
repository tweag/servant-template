module UI.Page.Home where

import Html
import UI
import qualified UI.Form.Account

data View = View

instance ToHtml View where
  toHtmlRaw = toHtml
  toHtml _ = toHtml view

view :: Html ()
view =
  mainLayout (h1_ "Tagger") $ do
    UI.Form.Account.registration
    UI.Form.Account.login
