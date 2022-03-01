{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import Infrastructure.Persistence.PostgresqlContentRepository (postgresContentRepository)
import Tagger.ContentRepository (hoist)

-- base
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString.Char8 (unpack)

-- hasql
import Hasql.Connection (acquire)

-- servant-auth-server
import Servant.Auth.Server (generateKey)

-- transformers
import Control.Monad.Trans.Except (runExceptT)

-- warp
import Network.Wai.Handler.Warp (run)

main:: IO ()
main = do
  -- TODO: use environment variables to pass in connection data
  connection <- acquire "host=localhost port=5432 dbname=tagger user=user password=password"
  key <- generateKey
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (run 8080 . app key)
    -- TODO: the whole hoist stuff could probably be managed in a better way
    (hoist (liftIO . (=<<) (either (fail . show) pure) . runExceptT) . postgresContentRepository <$> connection)
