{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.EncryptedPassword (EncryptedPassword, asBytestring, encryptPassword, validatePassword) where

import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)
import qualified Crypto.BCrypt as BCrypt (validatePassword)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.ByteString (ByteString)
import Data.Data (Proxy (Proxy))
import Data.OpenApi (ToSchema (declareNamedSchema))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType)

-- |
-- An 'EncryptedPassword' is a newtype wrapping a 'Bytestring'.
-- We do not export the constructor to enforce that an 'EncryptedPassword' is built using 'encryptPassword'
newtype EncryptedPassword = EncryptedPassword {asBytestring :: ByteString}
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype (DBEq, DBType)

instance FromJSON EncryptedPassword where
  parseJSON json = EncryptedPassword . encodeUtf8 <$> parseJSON json

instance ToJSON EncryptedPassword where
  toJSON (EncryptedPassword s) = toJSON $ decodeUtf8 s

instance ToSchema EncryptedPassword where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

-- |
-- encrypt a 'ByteString' into an 'EncryptedPassword' using bcrypt with 'fastBcryptHashingPolicy'
encryptPassword :: ByteString -> IO (Maybe EncryptedPassword)
encryptPassword password = fmap EncryptedPassword <$> hashPasswordUsingPolicy fastBcryptHashingPolicy password

-- |
-- Given an 'EncryptedPassword' and a 'ByteString' password, it checks whether the password is valid
validatePassword :: EncryptedPassword -> ByteString -> Bool
validatePassword (EncryptedPassword password) = BCrypt.validatePassword password
