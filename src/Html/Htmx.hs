module Html.Htmx
  ( module Html.Htmx,
    module Lucid.Htmx,
  )
where

import Data.String (IsString)
import Data.Text (intercalate)
import qualified Data.Text as T
import GHC.Exts (IsString (fromString))
import Lucid (Attribute)
import Lucid.Htmx

-- HXSwap
data HXSwapTarget
  = InnerHtml
  | OuterHtml
  | BeforeBegin
  | AfterBegin
  | BeforeEnd
  | AfterEnd
  | Delete
  | None

data Timing
  = Seconds Int
  | Milliseconds Int

instance Show Timing where
  show (Seconds s) = show s <> "s"
  show (Milliseconds ms) = show ms <> "ms"

data Direction = Top | Bottom

data HXSwapModifier
  = Swap Timing
  | Settle Timing
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

-- HX Target
type Selector = T.Text

data HXTarget
  = This
  | Target Selector
  | Closest Selector
  | Find Selector

instance IsString HXTarget where
  fromString = Target . T.pack . filter (not . (==) '#')

instance Show HXTarget where
  show This = "this"
  show (Target elementId) = T.unpack ("#" <> elementId)
  show (Closest query) = T.unpack $ "closest " <> query
  show (Find query) = T.unpack $ "find " <> query

hxTarget :: HXTarget -> Attribute
hxTarget = hxTarget_ . T.pack . show

-- HXTrigger
hxTrigger :: HXTrigger -> Attribute
hxTrigger = hxTrigger_ . T.pack . show

hxTriggers :: [HXTrigger] -> Attribute
hxTriggers = hxTrigger_ . intercalate ", " . map (T.pack . show)

data HXTrigger
  = BrowserEvt BrowserEvent [HXTriggerMods]
  | CustomEvent T.Text [HXTriggerMods]
  | Load [HXTriggerMods]
  | Revealed
  | Intersect [IntersectOptions]
  | Polling Timing

instance Show HXTrigger where
  show (BrowserEvt evt mods) = show evt <> " " <> unwords (map show mods)
  show (CustomEvent evtName mods) =
    T.unpack evtName <> unwords (map show mods) <> " " <> show (From $ NormalSelector "body")
  show (Load mods) = "load" <> unwords (map show mods)
  show Revealed = "revealed"
  show (Intersect opts) = "intersect" <> " " <> unwords (map show opts)
  show (Polling t) = "every " <> show t

-- | TODO Figure out whether it's possible to pull these in
-- as a dependency
data BrowserEvent
  = Clicked
  | MouseEnter

instance Show BrowserEvent where
  show Clicked = "clicked"
  show MouseEnter = "mouseenter"

type TriggerFilter = T.Text

data HXTriggerMods
  = Filter TriggerFilter
  | Once
  | Changed
  | Delay Timing
  | Throttle Timing
  | From ExtendedSelector
  | Target' Selector
  | Consume
  | Queue QueueOption

instance Show HXTriggerMods where
  show (Filter f) = T.unpack f
  show Once = "once"
  show Changed = "changed"
  show (Delay timing) = "delay:" <> show timing
  show (Throttle timing) = "throttle:" <> show timing
  show (From selector) = "from:" <> show selector
  show (Target' selector) = "target:" <> show selector
  show Consume = "consume"
  show (Queue options) = "queue:" <> show options

data ExtendedSelector
  = NormalSelector Selector
  | Document
  | Window
  | Closest' Selector
  | Find' Selector

instance Show ExtendedSelector where
  show (NormalSelector selector) = T.unpack selector
  show Document = "document"
  show Window = "window"
  show (Closest' query) = T.unpack $ "closest " <> query
  show (Find' query) = T.unpack $ "find " <> query

data QueueOption
  = QueueFirst
  | QueueLast
  | QueueAll

instance Show QueueOption where
  show QueueFirst = "first"
  show QueueLast = "last"
  show QueueAll = "all"

data IntersectOptions
  = Root Selector
  | Threshold Float

instance Show IntersectOptions where
  show (Root selector) = "root:" <> show selector
  show (Threshold f) = "threshold:" <> show f
