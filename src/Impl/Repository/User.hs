module Impl.Repository.User (inMemory, postgres) where

import Control.Monad.Trans.Except (ExceptT)
import Impl.Repository.User.Error (UserRepositoryError)
import qualified Impl.Repository.User.InMemory as IM
import qualified Impl.Repository.User.Postgres as PG
import qualified Infrastructure.Database as DB
import Tagger.Repository.User (UserRepository (..))

postgres :: DB.Handle -> UserRepository (ExceptT UserRepositoryError IO)
postgres = PG.repository

inMemory :: IM.Table -> UserRepository (ExceptT UserRepositoryError IO)
inMemory = IM.repository
