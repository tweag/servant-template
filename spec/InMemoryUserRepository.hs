{-# LANGUAGE OverloadedStrings #-}

module InMemoryUserRepository where

import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id(Id))
import Tagger.User (User(..))
import Tagger.UserRepository (SelectUserError(..), UserRepository(..))

-- base
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (TVar, atomically, readTVar, writeTVar)
import Prelude hiding (filter)

-- containers
import Data.Map.Lazy (Map, assocs, filter, insert, size)

-- hasql
import Hasql.Session (CommandError(ResultError), QueryError(QueryError), ResultError(ServerError))

-- mtl
import Control.Monad.Except (throwError)

-- postgres-error-codes
import PostgreSQL.ErrorCodes (unique_violation)

-- text
import Data.Text (Text)
import Data.Text.Encoding(encodeUtf8)

-- transformers
import Control.Monad.Trans.Except (ExceptT)

-- uuid
import Data.UUID.V4 (nextRandom)

inMemoryUserRepository :: TVar (Map (Id User) User) -> UserRepository (ExceptT QueryError IO)
inMemoryUserRepository userMap = UserRepository
  { getUserByName = inMemoryGetUserByName userMap
  , addUser       = inMemoryAddUser userMap
  }

inMemoryGetUserByName :: TVar (Map (Id User) User) -> Text -> ExceptT QueryError IO (Either SelectUserError (Id User, User))
inMemoryGetUserByName userMap name' = liftIO . atomically $ do
  users <- readTVar userMap
  let usersWithName = filter ((== name') . name) users
  case size usersWithName of
    0 -> pure $ Left NoUser
    1 -> pure . Right . head $ assocs usersWithName
    _ -> pure $ Left MoreThanOneUser

duplicateNameError :: Text -> QueryError
duplicateNameError name' = QueryError
  "insert user"
  []
  (ResultError $ ServerError
    unique_violation
    "duplicate key value violates unique constraint"
    (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
    Nothing)

inMemoryAddUser :: TVar (Map (Id User) User) -> Text -> EncryptedPassword -> ExceptT QueryError IO (Id User)
inMemoryAddUser userMap name' password' = do
  userId <- Id <$> liftIO nextRandom
  queryError <- liftIO . atomically $ do
    users <- readTVar userMap
    let usersWithName = filter ((== name') . name) users
    if   null usersWithName
    then writeTVar userMap (insert userId (User name' password') users) >> pure Nothing
    else pure . Just $ duplicateNameError name'
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure userId
