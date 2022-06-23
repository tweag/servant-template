{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.TaggerWeb where

import GHC.Generics (Generic)
import Servant (Handler, Headers, Header, addHeader)
import Servant.API (Get, JSON, Post, QueryParams, ReqBody, type (:>))
import Servant.API.Generic ((:-))
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Tagger.Content (Content)
import Tagger.ContentRepository (ContentRepository (addContentWithTags, selectUserContentsByTags))
import Tagger.Id (Id)
import Tagger.Owned (Owned)
import Tagger.Tag (Tag)
import Tagger.User (User)
import UI
import UI.ContentList
import Prelude hiding (getContents)
import qualified UI.Form.Content as UI.Form

newtype ContentList = ContentList [Owned (Content Tag)]

instance ToHtml ContentList where
  toHtmlRaw = toHtml
  toHtml (ContentList contents') = UI.ContentList.view contents'

newtype ContentCreated = ContentCreated (Id (Content Tag))

type ContentCreatedResponse =  Headers '[ Header "HX-Trigger" String ] ContentCreated

instance ToHtml ContentCreated where
  toHtmlRaw = toHtml
  toHtml (ContentCreated _) = toHtml $ do
    flash "Created successfully"
    UI.Form.newContent

-- |
-- The main endpoints of the application API
data TaggerWeb mode = TaggerWeb
  { contents ::
      mode
        :- "contents"
        :> QueryParams "tag" Tag
        :> Get '[HTML] ContentList,
    createContent ::
      mode
        :- "contents"
        :> ReqBody '[JSON] (Content Tag)
        :> Post '[HTML] ContentCreatedResponse
  }
  deriving stock (Generic)

taggerServerWeb :: Id User -> ContentRepository Handler -> TaggerWeb AsServer
taggerServerWeb userId contentRepository =
  TaggerWeb
    { contents = fmap ContentList . selectUserContentsByTags contentRepository userId,
      createContent = createContent' userId contentRepository
    }

createContent' :: Id User -> ContentRepository Handler -> Content Tag -> Handler ContentCreatedResponse
createContent' userId contentRepository =
  fmap (addHeader (show ContentsUpdated) . ContentCreated) . addContentWithTags contentRepository userId
