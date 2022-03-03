{-# LANGUAGE RankNTypes #-}

module Tagger.UserRepository where

-- bytestring
import Data.ByteString (ByteString)

-- text
import Data.Text (Text)

-- uuid
import Data.UUID (UUID)

newtype UserRepository m = UserRepository {addUser :: Text -> ByteString -> m UUID}

hoistUserRepository :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoistUserRepository f (UserRepository addUser') = UserRepository ((f .) . addUser')
