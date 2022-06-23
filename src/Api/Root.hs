{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api.Root where

import Lucid (ToHtml (toHtml, toHtmlRaw))
import Servant (Get, Server)
import Servant.HTML.Lucid
import Servant.API.ContentTypes (MimeUnrender, mimeUnrender)
import UI.Page.Home as Home

type RootAPI = Get '[HTML] Root

data Root = Root

instance ToHtml Root where
  toHtmlRaw = toHtml
  toHtml _ = toHtml Home.View

-- |
-- Instance needed to make use of servant-client for tests
instance MimeUnrender HTML Root where
  mimeUnrender _ _ = Right Root


rootServer :: Server RootAPI
rootServer = pure Root
