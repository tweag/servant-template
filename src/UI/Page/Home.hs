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
  div_ [class_ "container mx-auto"] $
    mainLayout $ do
      h1_ [class_ "text-center bg-blue-300 text-xl font-semibold"] "Tagger"

      div_ [id_ "accountForms", class_ "columns-2"] $ do
        div_ [class_ "w-1/2"] UI.Form.Account.registration
        div_ [class_ "w-1/2"] UI.Form.Account.login
