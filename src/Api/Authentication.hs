{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Authentication where

import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser (runAuthenticateUser))
import Infrastructure.Authentication.Login (Login(username, password))
import Infrastructure.Authentication.Token (Token(Token))
import Tagger.UserRepository (UserRepository(addUser))

-- base
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)

-- bcrypt
import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)

-- servant
import Servant (ReqBody, JSON, type (:>), Handler, err500, Post)
import Servant.API.Generic (type (:-))

-- servant-auth-server
import Servant.Auth.Server (makeJWT, JWTSettings, ThrowAll (throwAll))

-- servant-openapi3
import Servant.OpenApi (HasOpenApi(toOpenApi))

-- servant-server
import Servant.Server.Generic (AsServer)

-- uuid
import Data.UUID (UUID)
import Data.Text.Encoding (encodeUtf8)

data AuthenticationAPI mode = AuthenticationAPI
  { register :: mode :- "register" :> ReqBody '[JSON] Login :> Post '[JSON] UUID
  , login    :: mode :- "login"    :> ReqBody '[JSON] Login :> Post '[JSON] Token
  }
  deriving stock Generic

instance HasOpenApi AuthenticationAPI where
  toOpenApi _
    =  toOpenApi (Proxy :: Proxy ("register" :> ReqBody '[JSON] Login :> Post '[JSON] UUID))
    <> toOpenApi (Proxy :: Proxy ("login"    :> ReqBody '[JSON] Login :> Post '[JSON] Token))

authenticationServer :: JWTSettings -> AuthenticateUser Handler -> UserRepository Handler -> AuthenticationAPI AsServer
authenticationServer jwtSettings authenticateUser userRepository = AuthenticationAPI
  { register = registerEndpoint userRepository
  , login    = loginEndpoint jwtSettings authenticateUser
  }

registerEndpoint :: UserRepository Handler -> Login -> Handler UUID
registerEndpoint userRepository login' = do
  hashedPassword <- liftIO . hashPasswordUsingPolicy fastBcryptHashingPolicy . encodeUtf8 $ password login'
  case hashedPassword of
    -- TODO: log the hashing failure
    Nothing        -> throwAll err500
    Just password' -> addUser userRepository (username login') password'

-- TODO: login responds with 500 on wrong username or password
loginEndpoint :: JWTSettings -> AuthenticateUser Handler -> Login -> Handler Token
loginEndpoint jwtSettings authenticateUser login' = do
  user <- runAuthenticateUser authenticateUser login'
  eitherToken <- liftIO $ makeJWT user jwtSettings Nothing
  case eitherToken of
    -- TODO: log why the JWT generation failed
    Left  _     -> throwAll err500
    Right token -> pure $ Token token
