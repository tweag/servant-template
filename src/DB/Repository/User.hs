module DB.Repository.User (inMemory, postgres) where

import AppM (AppM')
import DB.Repository.User.InMemory qualified as IM
import DB.Repository.User.Postgres qualified as PG
import Tagger.Repository.User (UserRepository (..))

postgres :: UserRepository AppM'
postgres = PG.repository

inMemory :: IM.Table -> UserRepository AppM'
inMemory = IM.repository
