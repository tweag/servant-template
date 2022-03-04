{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Authentication where

import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser(runAuthenticateUser))
import Infrastructure.Authentication.Login (Login(username))
import Infrastructure.Authentication.PasswordManager (PasswordManager(generatePassword, generateToken))
import Infrastructure.Authentication.Token (Token)
import Tagger.User (asBytestring)
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

-- uuid
import Data.UUID (UUID)

data AuthenticationAPI mode = AuthenticationAPI
  { register :: mode :- "register" :> ReqBody '[JSON] Login :> Post '[JSON] UUID
  , login    :: mode :- "login"    :> ReqBody '[JSON] Login :> Post '[JSON] Token
  }
  deriving stock Generic

instance HasOpenApi AuthenticationAPI where
  toOpenApi _
    =  toOpenApi (Proxy :: Proxy ("register" :> ReqBody '[JSON] Login :> Post '[JSON] UUID))
    <> toOpenApi (Proxy :: Proxy ("login"    :> ReqBody '[JSON] Login :> Post '[JSON] Token))

authenticationServer :: PasswordManager Handler -> AuthenticateUser Handler -> UserRepository Handler -> AuthenticationAPI AsServer
authenticationServer passwordManager authenticateUser userRepository = AuthenticationAPI
  { register = registerEndpoint passwordManager userRepository
  , login    = loginEndpoint passwordManager authenticateUser
  }

registerEndpoint :: PasswordManager Handler -> UserRepository Handler -> Login -> Handler UUID
registerEndpoint passwordManager userRepository login' = do
  hashedPassword <- generatePassword passwordManager login'
  addUser userRepository (username login') (asBytestring hashedPassword)

loginEndpoint :: PasswordManager Handler -> AuthenticateUser Handler -> Login -> Handler Token
loginEndpoint passwordManager authenticateUser login' = do
  user <- runAuthenticateUser authenticateUser login'
  generateToken passwordManager user
