{-# LANGUAGE RecordWildCards #-}

module UI
  ( module UI,
    module UI.Layout,
    module Html,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Html
import UI.Layout
import qualified UI.Layout as Layout
import Prelude hiding (div)

data Event
  = ContentsUpdated

instance Show Event where
  show ContentsUpdated = "contentsUpdated"

flash :: Monad m => T.Text -> HtmlT m ()
flash msg =
  div_
    [ id_ flashAnchor',
      hxSwapOob Simple,
      class_ "bg-green-100 font-semibold"
    ]
    (p_ $ toHtml msg)
  where
    flashAnchor' = Layout.flashAnchor Layout.anchors

hxEvent :: Event -> HXTrigger
hxEvent evt = CustomEvent (T.pack . show $ evt) []

data InputProps = InputProps
  { inputName :: T.Text,
    inputLabel :: T.Text,
    inputType :: Maybe T.Text,
    inputPlaceholder :: Maybe T.Text,
    extraAttrs :: [Attribute]
  }

textInput :: InputProps -> Html ()
textInput InputProps {..} = do
  div_ [class_ "w-full flex flex-row mb-2"] $ do
    label_ [class_ "basis-1/6", for_ inputName] (toHtml inputLabel)
    input_ $
      [ id_ $ "input-" <> inputName,
        name_ inputName,
        placeholder_ $ fromMaybe "" inputPlaceholder,
        type_ $ fromMaybe "" inputType,
        class_ "w-full border border-blue-500 border-solid rounded p-1"
      ] <> extraAttrs

formButton :: T.Text -> Html ()
formButton label =
  div_ [class_ "flex flex-row mb-2"] $ do
    div_ [class_ "basis-1/6"] ""
    button_ [class_ "px-2 rounded bg-blue-300", type_ "submit"] (toHtml label)
