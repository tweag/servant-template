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


-- |
-- 'AuthenticateUser' is a service which exposes the ability to authenticate a 'User' providing her 'Login' credentials.
-- It is indexed by a context 'm' which wraps the results.
newtype AuthenticateUser m = AuthenticateUser {runAuthenticateUser :: Login -> m (Id User)}

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'AuthenticateUser' is operating
hoistAuthenticateUser :: (forall a. m a -> n a) -> AuthenticateUser m -> AuthenticateUser n
hoistAuthenticateUser f (AuthenticateUser auth) = AuthenticateUser $ f . auth

-- |
-- How 'authenticateUser' can actually fail
data AuthenticationError
  = AuthenticationSelectUserError SelectUserError -- ^ the provided 'Login' data do not correspond to a unique user
  | AuthenticationQueryError QueryError           -- ^ the interaction with the database somehow failed
  | AuthenticationPasswordVerificationFailed      -- ^ the password provided in the 'Login' data is not correct
  deriving Show

-- |
-- Concrete implementation of 'AuthenticateUser'.
-- Depends on a 'UserRepository' and a 'PasswordManager'
authenticateUser :: UserRepository (ExceptT QueryError IO) -> PasswordManager n -> Login -> ExceptT AuthenticationError IO (Id User)
authenticateUser userRepository passwordManager (Login username password) = do
  -- retrieve the user from the repository
  eitherIdAndUser <- withExceptT AuthenticationQueryError $ getUserByName userRepository username
  idAndUser       <- except $ first AuthenticationSelectUserError eitherIdAndUser
  -- check whether the provided password is the correct one
  if validatePassword passwordManager (snd idAndUser) password
  then pure $ fst idAndUser
  else throwE AuthenticationPasswordVerificationFailed
