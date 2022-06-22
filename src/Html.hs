{-# LANGUAGE RankNTypes #-}

module Html
  ( module Html,
    module Html.Htmx,
    module Lucid,
    module Lucid.Base,
  )
where

import Html.Htmx
import Lucid
import Lucid.Base (makeAttribute)

type Element m = Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()

type Element' m = Monad m => HtmlT m () -> HtmlT m ()

button :: Element a
button attributes = button_ `with` attributes

form :: Element a
form attributes = form_ `with` attributes

div :: Element a
div = div_
