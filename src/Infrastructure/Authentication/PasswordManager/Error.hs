module Infrastructure.Authentication.PasswordManager.Error where

import Crypto.JWT (Error)

-- |
-- How the 'PasswordManager' operations can fail
data PasswordManagerError
  = -- | there was an error while hashing the password
    FailedHashing
  | -- | there was an error while generating the authentication token
    FailedJWTCreation Error
  deriving stock (Show)
