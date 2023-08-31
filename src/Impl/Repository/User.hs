module Impl.Repository.User (inMemory, postgres) where

import Control.Monad.Trans.Except (ExceptT)
import Impl.Repository.User.Error (UserRepositoryError)
import Impl.Repository.User.InMemory qualified as IM
import Impl.Repository.User.Postgres qualified as PG
import Infrastructure.Database qualified as DB
import Tagger.Repository.User (UserRepository (..))

postgres :: DB.Handle -> UserRepository (ExceptT UserRepositoryError IO)
postgres = PG.repository

inMemory :: IM.Table -> UserRepository (ExceptT UserRepositoryError IO)
inMemory = IM.repository
