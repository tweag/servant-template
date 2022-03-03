{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.AuthenticateUser where

import Infrastructure.Authentication.Login (Login(Login))
import Infrastructure.Persistence.Queries (selectUserByName, SelectUserError)
import Infrastructure.Persistence.Serializer (unserializeUser)
import Infrastructure.Persistence.Schema (userPassword)
import Tagger.User (User)

-- base
import Data.Bifunctor (Bifunctor(first))

-- bcrypt
import Crypto.BCrypt (validatePassword)

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (run, QueryError)

-- text
import Data.Text.Encoding (encodeUtf8)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT), withExceptT, except, throwE)

newtype AuthenticateUser m = AuthenticateUser {runAuthenticateUser :: Login -> m User}

hoistAuthenticateUser :: (forall a. m a -> n a) -> AuthenticateUser m -> AuthenticateUser n
hoistAuthenticateUser f (AuthenticateUser auth) = AuthenticateUser $ f . auth

data AuthenticationError
  = AuthenticationSelectUserError SelectUserError
  | AuthenticationQueryError QueryError
  | AuthenticationPasswordVerificationFailed
  deriving Show

authenticateUser :: Connection -> Login -> ExceptT AuthenticationError IO User
authenticateUser connection (Login username password) = do
  eitherUser <- withExceptT AuthenticationQueryError $ ExceptT $ run (selectUserByName username) connection
  user       <- except $ first AuthenticationSelectUserError eitherUser
  if validatePassword (userPassword user) (encodeUtf8 password)
  then pure $ unserializeUser user
  else throwE AuthenticationPasswordVerificationFailed
