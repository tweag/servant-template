module Infrastructure.Persistence.PostgresUserRepository where

import qualified Infrastructure.Persistence.Queries as Query (addUser, selectUserByName)
import Infrastructure.Persistence.Serializer (serializeUser, unserializeUser)
import Infrastructure.Persistence.Schema (litUser, userId)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id(Id))
import Tagger.User (User(User))
import Tagger.UserRepository (SelectUserError, UserRepository(..))

-- base
import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, run)

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))

-- uuid
import Data.UUID.V4 (nextRandom)

-- |
-- A 'UserRepository' based on PostgreSQL
postgresUserRepository :: Connection -> UserRepository (ExceptT QueryError IO)
postgresUserRepository connection = UserRepository
  { getUserByName = postgresGetUserByName connection
  , addUser       = postgresAddUser connection
  }

postgresGetUserByName :: Connection -> Text -> ExceptT QueryError IO (Either SelectUserError (Id User, User))
postgresGetUserByName connection name = ExceptT $ do
  -- Try to retrieve the user with the provided name from the database
  eitherUser <- run (Query.selectUserByName name) connection
  -- Adjust the happy path format
  pure $ second (userId &&& unserializeUser) <$> eitherUser

postgresAddUser :: Connection -> Text -> EncryptedPassword -> ExceptT QueryError IO (Id User)
postgresAddUser connection name password = do
  -- Generate the UUID for the user
  userId' <- liftIO nextRandom
  -- Actually add the user to the database
  ExceptT $ run (Query.addUser . litUser $ serializeUser (Id userId') (User name password)) connection
  pure $ Id userId'
