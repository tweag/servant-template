{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagger.EncryptedPassword (EncryptedPassword, asBytestring, encryptPassword, validatePassword) where

-- base
import Data.Data (Proxy(Proxy))
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))

-- bcrypt
import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy)
import qualified Crypto.BCrypt as BCrypt (validatePassword)

-- bytestring
import Data.ByteString (ByteString)

-- openapi3
import Data.OpenApi (ToSchema(declareNamedSchema))

-- rel8
import Rel8 (DBEq, DBType)

-- text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

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
