{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.UserRepository where

import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id)
import Tagger.User (User)

-- text
import Data.Text (Text)

-- |
-- When we 'getUserByName' we expect only one 'User' in return.
-- 'SelectUserError' describes how this can fail
data SelectUserError
  = NoUser          -- ^ we are expecting one user, but actually no user is found
  | MoreThanOneUser -- ^ we are expecting one user, but actually more than one user is found
  deriving Show

-- |
-- A 'UserRespository' represents a collection of 'User's.
-- It is indexed by a context 'm' which wraps the results.
data UserRepository m = UserRepository
  { getUserByName :: Text -> m (Either SelectUserError (Id User, User)) -- ^ searches the repository for 'User's with the provided name
  , addUser       :: Text -> EncryptedPassword -> m (Id User)           -- ^ tries to add a user with the provided name and password
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'UserRepository' is operating
hoist :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoist f UserRepository{getUserByName, addUser} = UserRepository (f . getUserByName) ((f .) . addUser)
