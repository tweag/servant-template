{-# LANGUAGE OverloadedStrings #-}

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

-- bytestring
import Data.ByteString (isInfixOf)

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (QueryError (QueryError), run, CommandError (ResultError), ResultError (ServerError))

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT), withExceptT)

-- uuid
import Data.UUID.V4 (nextRandom)

-- We want to distinguish the `QueryError` coming from the violation of the "users_name_key" unique constraints
data UserRepositoryError
  = DuplicateUserName QueryError
  | OtherError QueryError
  deriving Show

liftAddUserError :: QueryError -> UserRepositoryError
liftAddUserError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "users_name_key" `isInfixOf` message = DuplicateUserName queryError
liftAddUserError queryError              = OtherError queryError

-- |
-- A 'UserRepository' based on PostgreSQL
postgresUserRepository :: Connection -> UserRepository (ExceptT UserRepositoryError IO)
postgresUserRepository connection = UserRepository
  { getUserByName = postgresGetUserByName connection
  , addUser       = postgresAddUser connection
  }

postgresGetUserByName :: Connection -> Text -> ExceptT UserRepositoryError IO (Either SelectUserError (Id User, User))
postgresGetUserByName connection name = withExceptT OtherError . ExceptT $ do
  -- Try to retrieve the user with the provided name from the database
  eitherUser <- run (Query.selectUserByName name) connection
  -- Adjust the happy path format
  pure $ second (userId &&& unserializeUser) <$> eitherUser

postgresAddUser :: Connection -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
postgresAddUser connection name password = do
  -- Generate the UUID for the user
  userId' <- liftIO nextRandom
  -- Actually add the user to the database, differentiating the `UserRepositoryError` cases
  withExceptT liftAddUserError . ExceptT $ run (Query.addUser . litUser $ serializeUser (Id userId') (User name password)) connection
  pure $ Id userId'
