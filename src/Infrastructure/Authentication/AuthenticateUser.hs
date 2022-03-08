{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.AuthenticateUser where

import Infrastructure.Authentication.Login (Login(Login))
import Infrastructure.Authentication.PasswordManager (PasswordManager(validatePassword))
import Infrastructure.Persistence.Queries (selectUserByName, SelectUserError)
import Infrastructure.Persistence.Schema (userId)
import Infrastructure.Persistence.Serializer (unserializeUser)
import Tagger.Id (Id)
import Tagger.User (User)

-- base
import Data.Bifunctor (Bifunctor(first))

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (run, QueryError)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT), withExceptT, except, throwE)

newtype AuthenticateUser m = AuthenticateUser {runAuthenticateUser :: Login -> m (Id User)}

hoistAuthenticateUser :: (forall a. m a -> n a) -> AuthenticateUser m -> AuthenticateUser n
hoistAuthenticateUser f (AuthenticateUser auth) = AuthenticateUser $ f . auth

data AuthenticationError
  = AuthenticationSelectUserError SelectUserError
  | AuthenticationQueryError QueryError
  | AuthenticationPasswordVerificationFailed
  deriving Show

authenticateUser :: PasswordManager m -> Connection -> Login -> ExceptT AuthenticationError IO (Id User)
authenticateUser passwordManager connection (Login username password) = do
  eitherUser <- withExceptT AuthenticationQueryError $ ExceptT $ run (selectUserByName username) connection
  user       <- except $ first AuthenticationSelectUserError eitherUser
  if validatePassword passwordManager (unserializeUser user) password
  then pure $ userId user
  else throwE AuthenticationPasswordVerificationFailed
