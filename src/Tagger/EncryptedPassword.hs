module Tagger.EncryptedPassword (EncryptedPassword, asBytestring, encryptPassword, validatePassword) where

import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)
import Crypto.BCrypt qualified as BCrypt (validatePassword)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.ByteString (ByteString)
import Data.Data (Proxy (Proxy))
import Data.OpenApi (ToSchema (declareNamedSchema))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType)
import Tagger.Authentication.Credentials (Password)
import Tagger.Authentication.Credentials qualified

-- | An 'EncryptedPassword' is a newtype wrapping a 'Bytestring'.
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

-- | Encrypt a 'Password' into an 'EncryptedPassword' using bcrypt with 'fastBcryptHashingPolicy'
encryptPassword :: Password -> IO (Maybe EncryptedPassword)
encryptPassword password =
  fmap EncryptedPassword <$> hashPasswordUsingPolicy fastBcryptHashingPolicy password.asBytestring

-- | Given an 'EncryptedPassword' and a 'Password', it checks whether the password is valid
validatePassword :: EncryptedPassword -> Password -> Bool
validatePassword encryptedPassword password =
  BCrypt.validatePassword
    encryptedPassword.asBytestring
    password.asBytestring
