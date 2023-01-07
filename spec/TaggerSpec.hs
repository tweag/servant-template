{-# LANGUAGE LambdaCase #-}

module TaggerSpec where

import API.Application (API, ApplicationAPI (..), app)
import API.Authentication (AuthenticationAPI (..))
import API.Tagger (TaggerAPI (..))
import Data.ByteString.Lazy (toStrict)
import Data.Either (isRight)
import Data.Proxy (Proxy (Proxy))
import Infrastructure.Authentication.Token (Token (Token))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (Status, forbidden403, unauthorized401)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import qualified Servant.Auth.Client.Internal as Servant (Token (Token))
import Servant.Client (ClientEnv, ClientM, HasClient (Client), baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Core (ClientError (..), responseStatusCode)
import Tagger.Authentication.Credentials (Credentials (Credentials), Password (Password))
import Tagger.Content (Content, createContent)
import Tagger.Id (Id)
import Tagger.Owned (Owned (Owned))
import Tagger.Tag (Tag (Tag))
import Tagger.User (User)
import Test.Hspec (Spec, around, describe, it, runIO, shouldMatchList, shouldSatisfy)
import TestServices (testServices)
import Prelude hiding (getContents)

withTaggerApp :: (Port -> IO ()) -> IO ()
withTaggerApp = testWithApplication $ app <$> testServices

hasStatus :: Status -> Either ClientError a -> Bool
hasStatus status = \case
  (Left (FailureResponse _ response)) -> responseStatusCode response == status
  _ -> False

toServantToken :: Token -> Servant.Token
toServantToken (Token token) = Servant.Token (toStrict token)

apiClient :: Client ClientM API
apiClient = client (Proxy :: Proxy API)

registerUser :: ClientEnv -> Credentials -> IO (Either ClientError (Id User))
registerUser env login' = runClientM ((register . authentication $ apiClient) login') env

loginUser :: ClientEnv -> Credentials -> IO (Either ClientError (Id User, Token))
loginUser env login' = do
  userId <- registerUser env login'
  token <- runClientM ((login . authentication $ apiClient) login') env
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
        _ <- registerUser (clientEnv port) (Credentials "marcosh" (Password "password"))
        response <- registerUser (clientEnv port) (Credentials "marcosh" (Password "password1"))
        response `shouldSatisfy` hasStatus forbidden403

      it "should register two users with different names" $ \port -> do
        _ <- registerUser (clientEnv port) (Credentials "marcosh" (Password "password"))
        response <- registerUser (clientEnv port) (Credentials "perons" (Password "password"))
        response `shouldSatisfy` isRight

    describe "login" $ do
      it "generates a token for a registered user" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        response <- loginUser (clientEnv port) loginData
        response `shouldSatisfy` isRight

      it "does not generate a token for a non registered user" $ \port -> do
        let loginOperation = login . authentication $ apiClient
            credentials = Credentials "marcosh" (Password "password")

        response <- runClientM (loginOperation credentials) (clientEnv port)
        response `shouldSatisfy` hasStatus unauthorized401

    describe "addContent" $ do
      it "allows a user to add a new content" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        token <- snd <$> successfullyLoginUser (clientEnv port) loginData
        let content = createContent "some content" [Tag "first tag", Tag "second tag"]
        response <- addUserContent (clientEnv port) token content
        response `shouldSatisfy` isRight

    describe "getContents" $ do
      it "retrieves all contents added by a user" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = createContent "some content" [Tag "first tag", Tag "second tag"]
        let content2 = createContent "other content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token content1
        _ <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token []
        case contents of
          Left _ -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1, Owned userId content2]

      it "retrieves all contents with a shared tag" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = createContent "some content" [Tag "first tag", Tag "second tag"]
        let content2 = createContent "other content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token content1
        _ <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "first tag"]
        case contents of
          Left _ -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1, Owned userId content2]

      it "retrieves only contents with a given tag" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = createContent "some content" [Tag "first tag", Tag "second tag"]
        let content2 = createContent "other content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token content1
        _ <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "second tag"]
        case contents of
          Left _ -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1]

      it "does not retrieve contents with non existing mix of tags" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (_, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = createContent "some content" [Tag "first tag", Tag "second tag"]
        let content2 = createContent "other content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token content1
        _ <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "second tag", Tag "third tag"]
        case contents of
          Left _ -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` []

      it "retrieves contents with all the required tags" $ \port -> do
        let loginData = Credentials "marcosh" (Password "password")
        (userId, token) <- successfullyLoginUser (clientEnv port) loginData
        let content1 = createContent "some content" [Tag "first tag", Tag "second tag"]
        let content2 = createContent "other content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token content1
        _ <- addUserContent (clientEnv port) token content2
        contents <- getUserContents (clientEnv port) token [Tag "first tag", Tag "second tag"]
        case contents of
          Left _ -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId content1]

      it "retrieves only contents from the requesting user" $ \port -> do
        let loginData1 = Credentials "marcosh" (Password "password")
        (userId1, token1) <- successfullyLoginUser (clientEnv port) loginData1
        let content1 = createContent "first content" [Tag "first tag", Tag "second tag"]
        let content2 = createContent "second content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token1 content1
        _ <- addUserContent (clientEnv port) token1 content2
        let loginData2 = Credentials "perons" (Password "password")
        (_, token2) <- successfullyLoginUser (clientEnv port) loginData2
        let content3 = createContent "third content" [Tag "first tag", Tag "second tag"]
        let content4 = createContent "fourth content" [Tag "first tag", Tag "third tag"]
        _ <- addUserContent (clientEnv port) token2 content3
        _ <- addUserContent (clientEnv port) token2 content4
        contents <- getUserContents (clientEnv port) token1 [Tag "first tag"]
        case contents of
          Left _ -> fail "unable to retrieve contents"
          Right ownedContent -> ownedContent `shouldMatchList` [Owned userId1 content1, Owned userId1 content2]
