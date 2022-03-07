{-# LANGUAGE RankNTypes #-}

module Tagger.UserRepository where


import Tagger.Id (Id)
import Tagger.User (User)

-- bytestring
import Data.ByteString (ByteString)

-- text
import Data.Text (Text)

newtype UserRepository m = UserRepository {addUser :: Text -> ByteString -> m (Id User)}

hoistUserRepository :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoistUserRepository f (UserRepository addUser') = UserRepository ((f .) . addUser')
