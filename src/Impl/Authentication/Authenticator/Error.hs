module Impl.Authentication.Authenticator.Error (Error (..)) where

import DB.Repository.User.Error (UserRepositoryError)
import DB.Queries (WrongNumberOfResults)

-- |
-- How 'authenticateUser' can actually fail
data Error
  = -- | the provided 'Credentials' data do not correspond to a unique user
    SelectUserError WrongNumberOfResults
  | -- | the interaction with the database somehow failed
    AuthQueryError UserRepositoryError
  | -- | the password provided in the 'Credentials' data is not correct
    PasswordVerificationFailed
  deriving (Show)
