{-# LANGUAGE OverloadedStrings #-}

module Tagger.Database
  ( Config (..),
    Handle,
    parseConfig,
    new,
    close,
    withHandle,
    runQuery,
  )
where

import Api.Config (dbConfig)
import qualified Api.Config as AppConfig
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Maybe (fromMaybe)
import Hasql.Connection (Connection, acquire)
import Hasql.Session (QueryError, Session, run)

newtype Config = Config
  { connectionString :: ByteString
  }

newtype Handle = Handle
  { dbConnection :: Connection
  }

new :: Config -> IO Handle
new config = do
  eConn <- acquire . connectionString $ config
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (pure . Handle)
    eConn

parseConfig :: AppConfig.Config -> Config
parseConfig =
  Config . (AppConfig.connectionString . dbConfig)

close :: Handle -> IO ()
close = undefined

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle = undefined

runQuery :: Handle -> Session a -> IO (Either QueryError a)
runQuery handle query =
  run query (dbConnection handle)
