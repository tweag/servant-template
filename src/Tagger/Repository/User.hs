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
  { -- | Searches the repository for 'User's with the provided name
    findByName :: Text -> m (Id User, User),
    -- | Adds a user with the provided name and password
    add :: Text -> EncryptedPassword -> m (Id User)
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'UserRepository' is operating
hoist :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoist f UserRepository {findByName, add} = UserRepository (f . findByName) ((f .) . add)
