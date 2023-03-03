{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Impl.Authentication.Authenticator (Error (..), authenticator) where

import Control.Monad.Trans.Except (ExceptT, throwE, withExceptT)
import Impl.Repository.User.Error (UserRepositoryError)
import Infrastructure.Authentication.PasswordManager (PasswordManager (validatePassword))
import Infrastructure.Persistence.Queries (WrongNumberOfResults)
import Tagger.Authentication.Authenticator (Authenticator (..))
import Tagger.Authentication.Credentials (Credentials (..))
import Tagger.Id (Id)
import Tagger.Repository.User as UserRepo
import Tagger.User (User)

authenticator ::
  UserRepository (ExceptT UserRepositoryError IO) ->
  PasswordManager n ->
  Authenticator (ExceptT Error IO)
authenticator repo pm =
  Authenticator
    { authUser = authenticateUser repo pm
    }

-- |
-- How 'authenticateUser' can actually fail
data Error
  = -- | the provided 'Credentials' data do not correspond to a unique user
    SelectUserError WrongNumberOfResults
  | -- | the interaction with the database somehow failed
    QueryError UserRepositoryError
  | -- | the password provided in the 'Credentials' data is not correct
    PasswordVerificationFailed
  deriving (Show)

-- |
-- Concrete implementation of 'AuthenticateUser'.
-- Depends on a 'UserRepository' and a 'PasswordManager'
authenticateUser :: UserRepository (ExceptT UserRepositoryError IO) -> PasswordManager n -> Credentials -> ExceptT Error IO (Id User)
authenticateUser userRepository passwordManager Credentials {username, password} = do
  (userId, user) <- withExceptT QueryError $ UserRepo.findByName userRepository username
  -- check whether the provided password is the correct one
  if validatePassword passwordManager user password
    then pure userId
    else throwE PasswordVerificationFailed
