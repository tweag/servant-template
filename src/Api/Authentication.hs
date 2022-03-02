{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Authentication where

import Infrastructure.Authentication.Login (Login(username))
import Infrastructure.Authentication.Token (Token(Token))
import Tagger.User (User(User))


-- base
import Control.Monad.IO.Class (liftIO)

-- servant
import Servant (ReqBody, JSON, type (:>), Verb, StdMethod (POST), Server, err500)

-- servant-auth-server
import Servant.Auth.Server (makeJWT, JWTSettings, ThrowAll (throwAll))

type AuthenticationAPI = "login" :> ReqBody '[JSON] Login :> Verb 'POST 200 '[JSON] Token

authenticationServer :: JWTSettings -> Server AuthenticationAPI
authenticationServer jwtSettings login = do
  eitherToken <- liftIO $ makeJWT (User $ username login) jwtSettings Nothing
  case eitherToken of
    -- TODO: log why the JWT generation failed
    Left  _     -> throwAll err500
    Right token -> pure $ Token token
