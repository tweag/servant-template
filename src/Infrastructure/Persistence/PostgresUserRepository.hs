{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Persistence.PostgresUserRepository where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, withExceptT)
import Data.ByteString (isInfixOf)
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import qualified Infrastructure.Database as DB
import Infrastructure.Persistence.Queries (WrongNumberOfResults)
import qualified Infrastructure.Persistence.Queries as Query
import Infrastructure.Persistence.Schema (litUser, userId)
import Infrastructure.Persistence.Serializer (serializeUser, unserializeUser)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id (Id))
import Tagger.Repository.User (UserRepository (..))
import Tagger.User (User (User))

-- We want to distinguish the `QueryError` coming from the violation of the "users_name_key" unique constraints
data UserRepositoryError
  = DuplicateUserName QueryError
  | UnexpectedNumberOfRows WrongNumberOfResults
  | OtherError QueryError
  deriving (Show)

-- |
-- A 'UserRepository' based on PostgreSQL
postgresUserRepository :: DB.Handle -> UserRepository (ExceptT UserRepositoryError IO)
postgresUserRepository handle =
  UserRepository
    { getUserByName = postgresGetUserByName handle,
      addUser = postgresAddUser handle
    }

postgresGetUserByName :: DB.Handle -> Text -> ExceptT UserRepositoryError IO (Id User, User)
postgresGetUserByName handle name = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserByName name)
  case eitherUser of
    Right usr -> pure (userId usr, unserializeUser usr)
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresAddUser :: DB.Handle -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
postgresAddUser handle name password = do
  -- Generate the UUID for the user
  userId' <- liftIO nextRandom
  let query = Query.addUser . litUser $ serializeUser (Id userId') (User name password)

  -- Actually add the user to the database, differentiating the `UserRepositoryError` cases
  runRepositoryQuery handle query
  pure $ Id userId'

-- | Run a query transforming a Hasql.QueryError into a UserRepositoryError as appropriate to the
-- domain.
runRepositoryQuery :: DB.Handle -> Session a -> ExceptT UserRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> UserRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "users_name_key" `isInfixOf` message = DuplicateUserName queryError
liftRepositoryError queryError = OtherError queryError
