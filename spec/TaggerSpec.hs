{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TaggerSpec where

import Api.Application (API, ApplicationAPI(..), app)
import Api.Authentication (AuthenticationAPI(..))
import Api.Tagger (TaggerAPI(..))
import Infrastructure.Authentication.Login (Login(Login))
import Infrastructure.Authentication.Token (Token(Token))
import Tagger.Content (Content(Content))
import Tagger.Owned (Owned(Owned))
import Tagger.Tag (Tag(Tag))
import Tagger.User (Password(Password))
import TestServices (testServices)

-- base
import Data.Either (isRight)
import Data.Proxy (Proxy(Proxy))
import Prelude hiding (getContents)


-- bytestring
import Data.ByteString.Lazy (toStrict)

-- hspec
import Test.Hspec (Spec, around, describe, it, runIO, shouldMatchList, shouldSatisfy)

-- http-client
import Network.HTTP.Client (defaultManagerSettings, newManager)

-- http-types
import Network.HTTP.Types.Status (Status, internalServerError500, unauthorized401)

-- servant-auth-client
import qualified Servant.Auth.Client.Internal as Servant (Token(Token))

-- servant-client-core
import Servant.Client.Core (ClientError(..), responseStatusCode)

-- servant-client
import Servant.Client (baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)

-- warp
import Network.Wai.Handler.Warp (Port, testWithApplication)

withTaggerApp :: (Port -> IO ()) -> IO ()
withTaggerApp = testWithApplication $ app <$> testServices

hasStatus :: Status -> Either ClientError a -> Bool
hasStatus status = \case
  (Left (FailureResponse _ response)) -> responseStatusCode response == status
  _                                   -> False

toServantToken :: Token -> Servant.Token
toServantToken (Token token) = Servant.Token (toStrict token)

spec :: Spec
spec = around withTaggerApp $ do
  let apiClient = client (Proxy :: Proxy API)
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

  describe "Tagger" $ do
    describe "register user" $ do
      it "should register a user" $ \port -> do
        response <- runClientM ((register . authentication $ apiClient) (Login "marcosh" (Password "password"))) (clientEnv port)
        response `shouldSatisfy` isRight

      it "should not register two users with the same name" $ \port -> do
        _ <- runClientM ((register . authentication $ apiClient) (Login "marcosh" (Password "password"))) (clientEnv port)
        response <- runClientM ((register . authentication $ apiClient) (Login "marcosh" (Password "password1"))) (clientEnv port)
        response `shouldSatisfy` hasStatus internalServerError500

      it "should register two users with different names" $ \port -> do
        _ <- runClientM ((register . authentication $ apiClient) (Login "marcosh" (Password "password"))) (clientEnv port)
        response <- runClientM ((register . authentication $ apiClient) (Login "perons" (Password "password"))) (clientEnv port)
        response `shouldSatisfy` isRight

    describe "login" $ do
      it "generates a token for a registered user" $ \port -> do
        let loginData = Login "marcosh" (Password "password")
        _        <- runClientM ((register . authentication $ apiClient) loginData) (clientEnv port)
        response <- runClientM ((login    . authentication $ apiClient) loginData) (clientEnv port)
        response `shouldSatisfy` isRight

      it "does not generate a token for a non registered user" $ \port -> do
        response <- runClientM ((login . authentication $ apiClient) (Login "marcosh" (Password "password"))) (clientEnv port)
        response `shouldSatisfy` hasStatus unauthorized401

    describe "addContent" $ do
      it "allows a user to add a new content" $ \port -> do
        let loginData = Login "marcosh" (Password "password")
        _           <- runClientM ((register . authentication $ apiClient) loginData) (clientEnv port)
        eitherToken <- runClientM ((login    . authentication $ apiClient) loginData) (clientEnv port)
        token       <- either (const $ fail "no token") pure eitherToken
        let content = Content "some content" [Tag "first tag", Tag "second tag"]
        response    <- runClientM ((addContent . tagger apiClient) (toServantToken token) content) (clientEnv port)
        response `shouldSatisfy` isRight

    describe "getContents" $ do
      it "retrieves all contents added by a user" $ \port -> do
        let loginData = Login "marcosh" (Password "password")
        eitherUserId <- runClientM ((register . authentication $ apiClient) loginData) (clientEnv port)
        userId       <- either (const $ fail "no user id") pure eitherUserId
        eitherToken  <- runClientM ((login    . authentication $ apiClient) loginData) (clientEnv port)
        token        <- either (const $ fail "no token") pure eitherToken
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content1) (clientEnv port)
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content2) (clientEnv port)
        contents     <- runClientM ((getContents . tagger apiClient) (toServantToken token) []) (clientEnv port)
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1, Owned userId content2]

      it "retrieves all contents with a shared tag" $ \port -> do
        let loginData = Login "marcosh" (Password "password")
        eitherUserId <- runClientM ((register . authentication $ apiClient) loginData) (clientEnv port)
        userId       <- either (const $ fail "no user id") pure eitherUserId
        eitherToken  <- runClientM ((login    . authentication $ apiClient) loginData) (clientEnv port)
        token        <- either (const $ fail "no token") pure eitherToken
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content1) (clientEnv port)
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content2) (clientEnv port)
        contents     <- runClientM ((getContents . tagger apiClient) (toServantToken token) [Tag "first tag"]) (clientEnv port)
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1, Owned userId content2]

      it "retrieves only contents with a given tag" $ \port -> do
        let loginData = Login "marcosh" (Password "password")
        eitherUserId <- runClientM ((register . authentication $ apiClient) loginData) (clientEnv port)
        userId       <- either (const $ fail "no user id") pure eitherUserId
        eitherToken  <- runClientM ((login    . authentication $ apiClient) loginData) (clientEnv port)
        token        <- either (const $ fail "no token") pure eitherToken
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content1) (clientEnv port)
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content2) (clientEnv port)
        contents     <- runClientM ((getContents . tagger apiClient) (toServantToken token) [Tag "second tag"]) (clientEnv port)
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1]

      it "does not retrieve contents with non existing mix of tags" $ \port -> do
        let loginData = Login "marcosh" (Password "password")
        eitherUserId <- runClientM ((register . authentication $ apiClient) loginData) (clientEnv port)
        _            <- either (const $ fail "no user id") pure eitherUserId
        eitherToken  <- runClientM ((login    . authentication $ apiClient) loginData) (clientEnv port)
        token        <- either (const $ fail "no token") pure eitherToken
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content1) (clientEnv port)
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content2) (clientEnv port)
        contents     <- runClientM ((getContents . tagger apiClient) (toServantToken token) [Tag "second tag", Tag "third tag"]) (clientEnv port)
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` []

      it "retrieves contents with all the required tags" $ \port -> do
        let loginData = Login "marcosh" (Password "password")
        eitherUserId <- runClientM ((register . authentication $ apiClient) loginData) (clientEnv port)
        userId            <- either (const $ fail "no user id") pure eitherUserId
        eitherToken  <- runClientM ((login    . authentication $ apiClient) loginData) (clientEnv port)
        token        <- either (const $ fail "no token") pure eitherToken
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content1) (clientEnv port)
        _            <- runClientM ((addContent  . tagger apiClient) (toServantToken token) content2) (clientEnv port)
        contents     <- runClientM ((getContents . tagger apiClient) (toServantToken token) [Tag "first tag", Tag "second tag"]) (clientEnv port)
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1]

      it "retrieves only contents from the requesting user" $ \port -> do
        let loginData1 = Login "marcosh" (Password "password")
        eitherUserId1 <- runClientM ((register . authentication $ apiClient) loginData1) (clientEnv port)
        userId1       <- either (const $ fail "no user id") pure eitherUserId1
        eitherToken1  <- runClientM ((login    . authentication $ apiClient) loginData1) (clientEnv port)
        token1        <- either (const $ fail "no token") pure eitherToken1
        let content1 = Content "first content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "second content" [Tag "first tag", Tag "third tag"]
        _             <- runClientM ((addContent  . tagger apiClient) (toServantToken token1) content1) (clientEnv port)
        _             <- runClientM ((addContent  . tagger apiClient) (toServantToken token1) content2) (clientEnv port)
        let loginData2 = Login "perons" (Password "password")
        eitherUserId2 <- runClientM ((register . authentication $ apiClient) loginData2) (clientEnv port)
        _             <- either (const $ fail "no user id") pure eitherUserId2
        eitherToken2  <- runClientM ((login    . authentication $ apiClient) loginData2) (clientEnv port)
        token2        <- either (const $ fail "no token") pure eitherToken2
        let content3 = Content "third content"  [Tag "first tag", Tag "second tag"]
        let content4 = Content "fourth content" [Tag "first tag", Tag "third tag"]
        _             <- runClientM ((addContent  . tagger apiClient) (toServantToken token2) content3) (clientEnv port)
        _             <- runClientM ((addContent  . tagger apiClient) (toServantToken token2) content4) (clientEnv port)
        contents      <- runClientM ((getContents . tagger apiClient) (toServantToken token1) [Tag "first tag"]) (clientEnv port)
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId1 content1, Owned userId1 content2]