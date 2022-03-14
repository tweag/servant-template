{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TaggerSpec where

import Api.Application (API, ApplicationAPI(..), app)
import Api.Authentication (AuthenticationAPI(..))
import Api.Tagger (TaggerAPI(..))
import Infrastructure.Authentication.Credentials (Credentials(Credentials), Password(Password))
import Infrastructure.Authentication.Token (Token(Token))
import Tagger.Content (Content(Content))
import Tagger.Id (Id)
import Tagger.Owned (Owned(Owned))
import Tagger.Tag (Tag(Tag))
import Tagger.User (User)
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
import Servant.Client (baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM, ClientEnv, HasClient (Client), ClientM)

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

apiClient :: Client ClientM API
apiClient = client (Proxy :: Proxy API)

registerUser :: ClientEnv -> Credentials -> IO (Either ClientError (Id User))
registerUser env login' = runClientM ((register . authentication $ apiClient) login') env

loginUser :: ClientEnv -> Credentials -> IO (Either ClientError (Id User, Token))
loginUser env login' = do
  userId <- registerUser env login'
  token  <- runClientM ((login    . authentication $ apiClient) login') env
  pure $ (,) <$> userId <*> token

successfullyLoginUser :: ClientEnv -> Credentials -> IO (Id User, Token)
successfullyLoginUser env login' = do
  eitherUserIdToken <- loginUser env login'
  either (const $ fail "no userId or token") pure eitherUserIdToken

addUserContent :: ClientEnv -> Token -> Content Tag -> IO (Either ClientError (Id (Content Tag)))
addUserContent env token content = runClientM ((addContent . tagger apiClient) (toServantToken token) content) env

getUserContents :: ClientEnv -> Token -> [Tag] -> IO (Either ClientError [Owned (Content Tag)])
getUserContents env token tags = runClientM ((getContents . tagger apiClient) (toServantToken token) tags) env

spec :: Spec
spec = around withTaggerApp $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

  describe "Tagger" $ do
    describe "register user" $ do
      it "should register a user" $ \port -> do
        response <- registerUser (clientEnv port) (Credentials "marcosh" (Password "password"))
        response `shouldSatisfy` isRight

      it "should not register two users with the same name" $ \port -> do
        _        <- registerUser (clientEnv port) (Credentials "marcosh" (Password "password"))
        response <- registerUser (clientEnv port) (Credentials "marcosh" (Password "password1"))
        response `shouldSatisfy` hasStatus internalServerError500

      it "should register two users with different names" $ \port -> do
        _        <- registerUser (clientEnv port) (Credentials "marcosh" (Password "password"))
        response <- registerUser (clientEnv port) (Credentials "perons"  (Password "password"))
        response `shouldSatisfy` isRight

    describe "login" $ do
      it "generates a token for a registered user" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        response <- loginUser (clientEnv port) loginData
        response `shouldSatisfy` isRight

      it "does not generate a token for a non registered user" $ \port -> do
        response <- runClientM ((login . authentication $ apiClient) (Credentials "marcosh" (Password "password"))) (clientEnv port)
        response `shouldSatisfy` hasStatus unauthorized401

    describe "addContent" $ do
      it "allows a user to add a new content" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        token <- snd <$> successfullyLoginUser (clientEnv port) loginData
        let content = Content "some content" [Tag "first tag", Tag "second tag"]
        response <- addUserContent (clientEnv port) token content
        response `shouldSatisfy` isRight

    describe "getContents" $ do
      it "retrieves all contents added by a user" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _        <- addUserContent (clientEnv port) token content1
        _        <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token []
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1, Owned userId content2]

      it "retrieves all contents with a shared tag" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _        <- addUserContent (clientEnv port) token content1
        _        <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "first tag"]
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1, Owned userId content2]

      it "retrieves only contents with a given tag" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _        <- addUserContent (clientEnv port) token content1
        _        <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "second tag"]
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1]

      it "does not retrieve contents with non existing mix of tags" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (_, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _        <- addUserContent (clientEnv port) token content1
        _        <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "second tag", Tag "third tag"]
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` []

      it "retrieves contents with all the required tags" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = Content "some content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "other content" [Tag "first tag", Tag "third tag"]
        _        <- addUserContent (clientEnv port) token content1
        _        <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "first tag", Tag "second tag"]
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1]

      it "retrieves only contents from the requesting user" $ \port -> do
        let loginData1 = Credentials "marcosh" (Password "password")
        (userId1, token1) <- successfullyLoginUser (clientEnv port) loginData1
        let content1 = Content "first content"  [Tag "first tag", Tag "second tag"]
        let content2 = Content "second content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token1 content1
        _ <- addUserContent (clientEnv port) token1 content2
        let loginData2 = Credentials "perons" (Password "password")
        (_, token2) <- successfullyLoginUser (clientEnv port) loginData2
        let content3 = Content "third content"  [Tag "first tag", Tag "second tag"]
        let content4 = Content "fourth content" [Tag "first tag", Tag "third tag"]
        _        <- addUserContent (clientEnv port) token2 content3
        _        <- addUserContent (clientEnv port) token2 content4
        contents <- getUserContents (clientEnv port) token1 [Tag "first tag"]
        case contents of
          Left _             -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId1 content1, Owned userId1 content2]
