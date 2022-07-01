{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Api.TaggerWeb where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (Handler, Header, Headers, addHeader)
import Servant.API (Get, JSON, Post, QueryParams, ReqBody, type (:>))
import Servant.API.Generic ((:-))
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Tagger.Content (Content)
import Tagger.ContentRepository (ContentRepository (addContentWithTags, selectUserContentsByTags))
import Tagger.Id (Id)
import Tagger.Owned (Owned)
import Tagger.Tag (Tag (..))
import qualified Tagger.Tag as Tag
import Tagger.User (User)
import UI
import UI.ContentList
import qualified UI.Form.Content as UI.Form
import Prelude hiding (getContents)

newtype ContentList = ContentList [Owned (Content Tag)]

instance ToHtml ContentList where
  toHtmlRaw = toHtml
  toHtml (ContentList contents') = toHtml $ UI.ContentList.view contents'

newtype ContentCreated = ContentCreated (Id (Content Tag))

newtype TagAdded = TagAdded Tag

type ContentCreatedResponse = Headers '[Header "HX-Trigger" String] ContentCreated

instance ToHtml ContentCreated where
  toHtmlRaw = toHtml
  toHtml (ContentCreated _) = toHtml $ do
    flash "Created successfully"
    UI.Form.newContent

instance ToHtml TagAdded where
  toHtmlRaw = toHtml
  toHtml (TagAdded tag) = do
    div_
      [ id_ "tag-list",
        hxSwapOob_ "beforeend"
      ]
      (div_ [class_ "border bg-blue-300 rounded-p1"] $ toHtml $ Tag.name tag <> " x")

    option_
      [ value_ (Tag.name tag),
        type_ "hidden",
        class_ "added-tag",
        selected_ "true"
      ]
      (toHtml $ Tag.name tag)

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
        :> Post '[HTML] ContentCreatedResponse,
    addTag ::
      mode
        :- "add-tag"
        :> ReqBody '[JSON] AddTagParams
        :> Post '[HTML] TagAdded
  }
  deriving stock (Generic)

data AddTagParams = AddTagParams {name :: T.Text}
  deriving (Generic, FromJSON)

taggerServerWeb :: Id User -> ContentRepository Handler -> TaggerWeb AsServer
taggerServerWeb userId contentRepository =
  TaggerWeb
    { contents = fmap ContentList . selectUserContentsByTags contentRepository userId,
      createContent = createContent' userId contentRepository,
      addTag = addTag'
    }

createContent' :: Id User -> ContentRepository Handler -> Content Tag -> Handler ContentCreatedResponse
createContent' userId contentRepository =
  fmap (addHeader (show ContentsUpdated) . ContentCreated) . addContentWithTags contentRepository userId

addTag' :: AddTagParams -> Handler TagAdded
addTag' (AddTagParams tag) = pure . TagAdded . Tag $ tag
