{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Authentication where

import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser (runAuthenticateUser))
import Infrastructure.Authentication.Login (Login)
import Infrastructure.Authentication.Token (Token(Token))

-- base
import Control.Monad.IO.Class (liftIO)

-- servant
-- servant
import Servant (ReqBody, JSON, type (:>), Verb, StdMethod (POST), Server, Handler, err500)

-- servant-auth-server
import Servant.Auth.Server (makeJWT, JWTSettings, ThrowAll (throwAll))

type AuthenticationAPI = "login" :> ReqBody '[JSON] Login :> Verb 'POST 200 '[JSON] Token

authenticationServer :: JWTSettings -> AuthenticateUser Handler -> Server AuthenticationAPI
authenticationServer jwtSettings authenticateUser login = do
  user <- runAuthenticateUser authenticateUser login
  eitherToken <- liftIO $ makeJWT user jwtSettings Nothing
  case eitherToken of
    -- TODO: log why the JWT generation failed
    Left  _     -> throwAll err500
    Right token -> pure $ Token token
