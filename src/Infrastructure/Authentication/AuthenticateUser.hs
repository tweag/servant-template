{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.AuthenticateUser where

import Infrastructure.Authentication.Login (Login(Login))
import Infrastructure.Authentication.PasswordManager (PasswordManager(validatePassword))
import Tagger.Id (Id)
import Tagger.User (User)
import Tagger.UserRepository (SelectUserError, UserRepository(getUserByName))

-- base
import Data.Bifunctor (Bifunctor(first))

-- hasql
import Hasql.Session (QueryError)

-- transformers
import Control.Monad.Trans.Except (ExceptT, withExceptT, except, throwE)

newtype AuthenticateUser m = AuthenticateUser {runAuthenticateUser :: Login -> m (Id User)}

hoistAuthenticateUser :: (forall a. m a -> n a) -> AuthenticateUser m -> AuthenticateUser n
hoistAuthenticateUser f (AuthenticateUser auth) = AuthenticateUser $ f . auth

data AuthenticationError
  = AuthenticationSelectUserError SelectUserError
  | AuthenticationQueryError QueryError
  | AuthenticationPasswordVerificationFailed
  deriving Show

authenticateUser :: UserRepository (ExceptT QueryError IO) -> PasswordManager n -> Login -> ExceptT AuthenticationError IO (Id User)
authenticateUser userRepository passwordManager (Login username password) = do
  eitherIdAndUser <- withExceptT AuthenticationQueryError $ getUserByName userRepository username
  idAndUser       <- except $ first AuthenticationSelectUserError eitherIdAndUser
  if validatePassword passwordManager (snd idAndUser) password
  then pure $ fst idAndUser
  else throwE AuthenticationPasswordVerificationFailed
