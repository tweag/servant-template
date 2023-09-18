module DB.Repository.User.InMemory (Table, repository) where

import App.Error (AppError (..))
import AppM (AppM, AppM')
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Lazy (Map, assocs, filter, insert, size)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4 (nextRandom)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError))
import DB.Repository.User.Error (UserRepositoryError (..))
import DB.Queries (WrongNumberOfResults (..))
import PostgreSQL.ErrorCodes (unique_violation)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id (Id))
import Tagger.Repository.User (UserRepository (..))
import Tagger.User (User (..))
import Prelude hiding (filter)

type Table = TVar (Map (Id User) User)

repository :: Table -> UserRepository AppM'
repository userMap =
  UserRepository
    { findByName = inMemoryGetUserByName userMap,
      add = inMemoryAddUser userMap
    }

inMemoryGetUserByName :: Table -> Text -> AppM (Id User, User)
inMemoryGetUserByName userMap name' = do
  users <- liftIO $ readTVarIO userMap
  let usersWithName = filter (\u -> u.name == name') users
  case size usersWithName of
    0 -> throwError $ UserRepositoryErr (UnexpectedNumberOfRows NoResults)
    1 -> pure . head . assocs $ usersWithName
    _ -> throwError $ UserRepositoryErr (UnexpectedNumberOfRows MoreThanOneResult)

duplicateNameError :: Text -> AppError
duplicateNameError name' =
  UserRepositoryErr $
    DuplicateUserName $
      QueryError
        "insert user"
        []
        ( ResultError $
            ServerError
              unique_violation
              "duplicate key value violates unique constraint"
              (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
              Nothing
              Nothing
        )

inMemoryAddUser :: Table -> Text -> EncryptedPassword -> AppM (Id User)
inMemoryAddUser userMap name' password' = do
  userId <- Id <$> liftIO nextRandom
  queryError <- liftIO . atomically $ do
    users <- readTVar userMap
    let usersWithName = filter ((== name') . name) users
    if null usersWithName
      then writeTVar userMap (insert userId (User name' password') users) >> pure Nothing
      else pure . Just $ duplicateNameError name'
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure userId
