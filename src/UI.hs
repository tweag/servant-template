module UI
  ( module UI,
    module UI.Layout,
    module Html,
  )
where

import qualified Data.Text as T
import Html
import Prelude hiding (div)
import UI.Layout
import qualified UI.Layout as Layout

data Event =
  ContentsUpdated

instance Show Event where
  show ContentsUpdated = "contentsUpdated"

flash :: Monad m => T.Text -> HtmlT m ()
flash = div_ [id_ flashAnchor', makeAttribute "hx-swap-oob" "true"] . toHtml
  where
    flashAnchor' = Layout.flashAnchor Layout.anchors

hxEvent :: Event -> HXTrigger
hxEvent evt = CustomEvent (T.pack . show $ evt) []
