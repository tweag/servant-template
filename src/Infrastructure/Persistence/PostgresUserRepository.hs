module Infrastructure.Persistence.PostgresUserRepository where

import qualified Infrastructure.Persistence.Queries as Query (addUser, selectUserByName)
import Infrastructure.Persistence.Serializer (serializeUser, unserializeUser)
import Infrastructure.Persistence.Schema (litUser)
import Tagger.Id (Id(Id))
import Tagger.User (Password(Password), User(User))
import Tagger.UserRepository (SelectUserError, UserRepository(..))

-- base
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)

-- bytestring
import Data.ByteString (ByteString)

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, run)

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))

-- uuid
import Data.UUID.V4 (nextRandom)

postgresUserRepository :: Connection -> UserRepository (ExceptT QueryError IO)
postgresUserRepository connection = UserRepository
  { getUserByName = postgresGetUserByName connection
  , addUser       = postgresAddUser connection
  }

postgresGetUserByName :: Connection -> Text -> ExceptT QueryError IO (Either SelectUserError User)
postgresGetUserByName connection name = fmap (second unserializeUser) <$> ExceptT $ run (Query.selectUserByName name) connection

postgresAddUser :: Connection -> Text -> ByteString -> ExceptT QueryError IO (Id User)
postgresAddUser connection name password = do
    userId <- liftIO nextRandom
    ExceptT $ run (Query.addUser . litUser $ serializeUser (Id userId) (User name (Password password))) connection
    pure $ Id userId
