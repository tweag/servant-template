module UI
  ( module UI,
    module UI.Layout,
    module Html,
  )
where

import Data.Text
import Html
import Prelude hiding (div)
import UI.Layout

flash :: Text -> Html ()
flash = div [id_ "flash"] . toHtml
