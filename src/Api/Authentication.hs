{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Authentication where

import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser(runAuthenticateUser))
import Infrastructure.Authentication.Login (Login(username))
import Infrastructure.Authentication.PasswordManager (PasswordManager(generatePassword, generateToken))
import Infrastructure.Authentication.Token (Token)
import Tagger.Id (Id)
import Tagger.User (User)
import Tagger.UserRepository (UserRepository(addUser))

-- base
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)

-- servant
import Servant (ReqBody, JSON, type (:>), Handler, Post)
import Servant.API.Generic (type (:-))

-- servant-openapi3
import Servant.OpenApi (HasOpenApi(toOpenApi))

-- servant-server
import Servant.Server.Generic (AsServer)

-- |
-- The endpoints required to perform authentication
data AuthenticationAPI mode = AuthenticationAPI
  { register :: mode :- "register" :> ReqBody '[JSON] Login :> Post '[JSON] (Id User) -- ^ Given some 'Login' data, registers a new 'User'
  , login    :: mode :- "login"    :> ReqBody '[JSON] Login :> Post '[JSON] Token     -- ^ Given some 'Login' data, generates an authentication token
  }
  deriving stock Generic

instance HasOpenApi AuthenticationAPI where
  toOpenApi _
    =  toOpenApi (Proxy :: Proxy ("register" :> ReqBody '[JSON] Login :> Post '[JSON] (Id User)))
    <> toOpenApi (Proxy :: Proxy ("login"    :> ReqBody '[JSON] Login :> Post '[JSON] Token))

authenticationServer :: PasswordManager Handler -> AuthenticateUser Handler -> UserRepository Handler -> AuthenticationAPI AsServer
authenticationServer passwordManager authenticateUser userRepository = AuthenticationAPI
  { register = registerEndpoint passwordManager userRepository
  , login    = loginEndpoint passwordManager authenticateUser
  }

registerEndpoint :: PasswordManager Handler -> UserRepository Handler -> Login -> Handler (Id User)
registerEndpoint passwordManager userRepository login' = do
  -- hash the password
  hashedPassword <- generatePassword passwordManager login'
  -- store the new user into the database
  addUser userRepository (username login') hashedPassword

loginEndpoint :: PasswordManager Handler -> AuthenticateUser Handler -> Login -> Handler Token
loginEndpoint passwordManager authenticateUser login' = do
  -- try to authenticate the user
  user <- runAuthenticateUser authenticateUser login'
  -- if the user authenticated, generate an authentication token
  generateToken passwordManager user
