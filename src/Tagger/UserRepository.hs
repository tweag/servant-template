{-# LANGUAGE RankNTypes #-}

module Tagger.UserRepository where

import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id)
import Tagger.User (User)

-- text
import Data.Text (Text)

data SelectUserError
  = NoUser
  | MoreThanOneUser
  deriving Show

data UserRepository m = UserRepository
  { getUserByName :: Text -> m (Either SelectUserError (Id User, User))
  , addUser       :: Text -> EncryptedPassword -> m (Id User)
  }

hoistUserRepository :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoistUserRepository f (UserRepository getUserByName' addUser') = UserRepository (f . getUserByName') ((f .) . addUser')
