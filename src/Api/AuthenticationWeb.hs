{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.AuthenticationWeb where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser (runAuthenticateUser))
import Infrastructure.Authentication.Credentials (Credentials (username))
import Infrastructure.Authentication.PasswordManager (PasswordManager (generatePassword, generateToken))
import Infrastructure.Authentication.Token (Token, toText)
import Servant (Handler, Header, Headers, JSON, Post, ReqBody, addHeader, type (:>))
import Servant.API.Generic (type (:-))
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Tagger.Id (Id (..))
import Tagger.User (User)
import Tagger.UserRepository (UserRepository (addUser))
import UI
import qualified UI.Form.Account as AccountForms
import UI.Layout
import qualified UI.Dashboard

-- |
-- The endpoints required to perform authentication
data AuthenticationWeb mode = AuthenticationWeb
  { register :: mode :- "register" :> ReqBody '[JSON] Credentials :> Post '[HTML] RegistrationResponse,
    signin :: mode :- "signin" :> ReqBody '[JSON] Credentials :> Post '[HTML] SigninResponse
  }
  deriving stock (Generic)

newtype RegistrationResponse = RegistrationResponse (Id User)
  deriving stock (Show)
  deriving newtype (ToSchema, ToJSON, FromJSON)

type SigninResponse =
  Headers
    '[ Header "HX-Trigger" T.Text,
       Header "HX-Retarget" T.Text,
       Header "HX-Push" T.Text
     ]
    SignInContent

data SignInContent = SignInContent

instance ToHtml RegistrationResponse where
  toHtmlRaw = toHtml
  toHtml _ = toHtml $ do
    flash "User registered successfully!"
    AccountForms.registration

instance ToHtml SignInContent where
  toHtmlRaw = toHtml
  toHtml _ = do
    flash "Logged in!"
    UI.Dashboard.view

authenticationServerWeb ::
  PasswordManager Handler ->
  AuthenticateUser Handler ->
  UserRepository Handler ->
  AuthenticationWeb AsServer
authenticationServerWeb passwordManager authenticateUser userRepository =
  let authenticator = authenticate passwordManager authenticateUser
   in AuthenticationWeb
        { register = registerEndpoint passwordManager userRepository,
          signin = signinEndpoint authenticator
        }

registerEndpoint :: PasswordManager Handler -> UserRepository Handler -> Credentials -> Handler RegistrationResponse
registerEndpoint passwordManager userRepository credentials = do
  -- hash the password
  hashedPassword <- generatePassword passwordManager credentials
  -- store the new user into the database
  RegistrationResponse <$> addUser userRepository (username credentials) hashedPassword

signinEndpoint :: (Credentials -> Handler Token) -> Credentials -> Handler SigninResponse
signinEndpoint authenticator credentials = do
  token <- authenticator credentials
  let loggedInEvent = "{\"loggedIn\":\"" <> toText token <> "\"}"
  let contentTarget = "#" <> UI.Layout.mainContentAnchor UI.Layout.anchors
  return $
    addHeader loggedInEvent
      . addHeader contentTarget
      . addHeader "/dashboard"
      $ SignInContent

authenticate :: PasswordManager Handler -> AuthenticateUser Handler -> Credentials -> Handler Token
authenticate passwordManager authenticateUser credentials = do
  -- try to authenticate the user
  userId <- runAuthenticateUser authenticateUser credentials
  -- if the user authenticated, generate an authentication token
  generateToken passwordManager userId
