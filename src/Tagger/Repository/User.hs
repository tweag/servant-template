{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.Repository.User (UserRepository (..), hoist) where

import Data.Text (Text)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id)
import Tagger.User (User)

-- |
-- A 'UserRespository' represents a collection of 'User's.
-- It is indexed by a context 'm' which wraps the results.
data UserRepository m = UserRepository
  { -- | searches the repository for 'User's with the provided name
    getUserByName :: Text -> m (Id User, User),
    -- | tries to add a user with the provided name and password
    addUser :: Text -> EncryptedPassword -> m (Id User)
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'UserRepository' is operating
hoist :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoist f UserRepository {getUserByName, addUser} = UserRepository (f . getUserByName) ((f .) . addUser)
