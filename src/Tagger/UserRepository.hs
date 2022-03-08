{-# LANGUAGE RankNTypes #-}

module Tagger.UserRepository where


import Tagger.Id (Id)
import Tagger.User (User)

-- bytestring
import Data.ByteString (ByteString)

-- text
import Data.Text (Text)

data SelectUserError
  = NoUser
  | MoreThanOneUser
  deriving Show

data UserRepository m = UserRepository
  { getUserByName :: Text -> m (Either SelectUserError User)
  , addUser       :: Text -> ByteString -> m (Id User)
  }

hoistUserRepository :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoistUserRepository f (UserRepository getUserByName' addUser') = UserRepository (f . getUserByName') ((f .) . addUser')
