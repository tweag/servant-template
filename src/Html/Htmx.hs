{-# LANGUAGE OverloadedStrings #-}

module Html.Htmx
  ( module Html.Htmx,
    module Lucid.Htmx,
  )
where

import qualified Data.Text as T
import Lucid
import Lucid.Htmx

data HXSwapTarget
  = InnerHtml
  | OuterHtml
  | BeforeBegin
  | AfterBegin
  | BeforeEnd
  | AfterEnd
  | Delete
  | None

type Seconds = Double

data Direction = Top | Bottom

data HXSwapModifier
  = Swap Seconds
  | Settle Seconds
  | Scroll Direction

hxSwap :: HXSwapTarget -> [HXSwapModifier] -> Attribute
hxSwap target mods = hxSwap_ qualifiers
  where
    qualifiers = T.pack $ show target <> " " <> unwords (map show mods)

instance Show HXSwapTarget where
  show InnerHtml = "innerHTML"
  show OuterHtml = "outerHTML"
  show BeforeBegin = "beforebegin"
  show AfterBegin = "afterbegin"
  show BeforeEnd = "beforeend"
  show AfterEnd = "afterend"
  show Delete = "delete"
  show None = "none"

instance Show HXSwapModifier where
  show (Swap s) = "swap:" <> show s <> "s"
  show (Settle s) = "settle:" <> show s <> "s"
  show (Scroll d) = "scroll:" <> show d

instance Show Direction where
  show Top = "top"
  show Bottom = "bottom"
