{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database
  ( Config (..),
    Handle,
    withHandle,
    runQuery,
  )
where

import qualified Api.Config as AppConfig
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Maybe (fromMaybe)
import Hasql.Connection (Connection, acquire, release)
import Hasql.Session (QueryError, Session, run)
import Control.Exception (bracket)

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
  Config . (AppConfig.connectionString . AppConfig.database)

close :: Handle -> IO ()
close = release . dbConnection

withHandle :: AppConfig.Config -> (Handle -> IO a) -> IO a
withHandle config f = do
  bracket
    (new . parseConfig $ config)
    close
    f


runQuery :: Handle -> Session a -> IO (Either QueryError a)
runQuery handle query =
  run query (dbConnection handle)
