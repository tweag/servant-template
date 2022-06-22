{-# LANGUAGE DataKinds #-}

module Api.Root where

import Lucid (ToHtml (toHtml, toHtmlRaw))
import Servant (Get, Server)
import Servant.HTML.Lucid
import UI.Page.Home as Home

type RootAPI = Get '[HTML] Root

data Root = Root

instance ToHtml Root where
  toHtmlRaw = toHtml
  toHtml _ = toHtml Home.View

rootServer :: Server RootAPI
rootServer = pure Root
