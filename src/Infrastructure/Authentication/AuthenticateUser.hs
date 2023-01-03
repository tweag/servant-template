{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.AuthenticateUser where

import Control.Monad.Trans.Except (ExceptT, throwE, withExceptT)
import Infrastructure.Authentication.Credentials (Credentials (..))
import Infrastructure.Authentication.PasswordManager (PasswordManager (validatePassword))
import Infrastructure.Persistence.PostgresUserRepository (UserRepositoryError)
import Infrastructure.Persistence.Queries (WrongNumberOfRows)
import Tagger.Id (Id)
import Tagger.User (User)
import Tagger.UserRepository (UserRepository (getUserByName))

-- |
-- 'AuthenticateUser' is a service which exposes the ability to authenticate a 'User' providing her 'Credentials'.
-- It is indexed by a context 'm' which wraps the results.
newtype AuthenticateUser m = AuthenticateUser {runAuthenticateUser :: Credentials -> m (Id User)}

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'AuthenticateUser' is operating
hoist :: (forall a. m a -> n a) -> AuthenticateUser m -> AuthenticateUser n
hoist f (AuthenticateUser auth) = AuthenticateUser $ f . auth

-- |
-- How 'authenticateUser' can actually fail
data AuthenticationError
  = -- | the provided 'Credentials' data do not correspond to a unique user
    AuthenticationSelectUserError WrongNumberOfRows
  | -- | the interaction with the database somehow failed
    AuthenticationQueryError UserRepositoryError
  | -- | the password provided in the 'Credentials' data is not correct
    AuthenticationPasswordVerificationFailed
  deriving (Show)

-- |
-- Concrete implementation of 'AuthenticateUser'.
-- Depends on a 'UserRepository' and a 'PasswordManager'
authenticateUser :: UserRepository (ExceptT UserRepositoryError IO) -> PasswordManager n -> Credentials -> ExceptT AuthenticationError IO (Id User)
authenticateUser userRepository passwordManager Credentials {username, password} = do
  (userId, user) <- withExceptT AuthenticationQueryError $ getUserByName userRepository username
  -- check whether the provided password is the correct one
  if validatePassword passwordManager user password
    then pure userId
    else throwE AuthenticationPasswordVerificationFailed
