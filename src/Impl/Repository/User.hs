module Impl.Repository.User (inMemory, postgres) where

import AppM (AppM')
import Impl.Repository.User.InMemory qualified as IM
import Impl.Repository.User.Postgres qualified as PG
import Tagger.Repository.User (UserRepository (..))

postgres :: UserRepository AppM'
postgres = PG.repository

inMemory :: IM.Table -> UserRepository AppM'
inMemory = IM.repository
