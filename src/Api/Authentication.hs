{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Authentication where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser (runAuthenticateUser))
import Infrastructure.Authentication.Credentials (Credentials (username))
import Infrastructure.Authentication.PasswordManager (PasswordManager (generatePassword, generateToken))
import Infrastructure.Authentication.Token (Token)
import Servant (Handler, JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (type (:-))
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server.Generic (AsServer)
import Tagger.Id (Id (..))
import Tagger.User (User)
import Tagger.UserRepository (UserRepository (addUser))

type Register =
  "register"
    :> ReqBody '[JSON] Credentials
    :> Post '[JSON] RegistrationResponse

type Login =
  "login"
    :> ReqBody '[JSON] Credentials
    :> Post '[JSON] LoginResponse

newtype RegistrationResponse = RegistrationResponse {idFromResponse :: Id User}
  deriving stock (Show)
  deriving newtype (ToSchema, ToJSON, FromJSON)

newtype LoginResponse = LoginResponse {tokenFromResponse :: Token}
  deriving stock (Show)
  deriving newtype (ToSchema, ToJSON, FromJSON)

-- |
-- The endpoints required to perform authentication
data AuthenticationAPI mode = AuthenticationAPI
  { -- | Given some 'Login' data, registers a new 'User'
    register :: mode :- Register,
    -- | Given some 'Login' data, generates an authentication token
    login :: mode :- Login
  }
  deriving stock (Generic)

instance HasOpenApi AuthenticationAPI where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy Register)
      <> toOpenApi (Proxy :: Proxy Login)

authenticationServer ::
  PasswordManager Handler ->
  AuthenticateUser Handler ->
  UserRepository Handler ->
  AuthenticationAPI AsServer
authenticationServer passwordManager authenticateUser userRepository =
  let authenticator = authenticate passwordManager authenticateUser
   in AuthenticationAPI
        { register = registerEndpoint passwordManager userRepository,
          login = loginEndpoint authenticator
        }

registerEndpoint :: PasswordManager Handler -> UserRepository Handler -> Credentials -> Handler RegistrationResponse
registerEndpoint passwordManager userRepository credentials = do
  -- hash the password
  hashedPassword <- generatePassword passwordManager credentials
  -- store the new user into the database
  RegistrationResponse <$> addUser userRepository (username credentials) hashedPassword

loginEndpoint :: (Credentials -> Handler Token) -> Credentials -> Handler LoginResponse
loginEndpoint authenticator credentials = do
  token <- authenticator credentials
  pure $ LoginResponse token

authenticate :: PasswordManager Handler -> AuthenticateUser Handler -> Credentials -> Handler Token
authenticate passwordManager authenticateUser credentials = do
  -- try to authenticate the user
  userId <- runAuthenticateUser authenticateUser credentials
  -- if the user authenticated, generate an authentication token
  generateToken passwordManager userId
