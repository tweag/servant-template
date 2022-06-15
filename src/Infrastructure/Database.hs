{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database (Config (..), Handle, withHandle, runQuery) where

import qualified Api.Config as AppConfig
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Hasql.Connection (acquire, release, settings)
import qualified Hasql.Connection as Hasql
import Hasql.Session (QueryError, Session, run)

newtype Config = Config
  { connectionString :: Hasql.Settings
  }

newtype Handle = Handle
  { dbConnection :: Hasql.Connection
  }

new :: Config -> IO Handle
new config = do
  eConn <- acquire . connectionString $ config
  either
    (fail . BS.unpack . fromMaybe "unable to connect to the database")
    (pure . Handle)
    eConn

parseConfig :: AppConfig.Config -> Config
parseConfig =
  Config . (mkHasqlSettings . AppConfig.database)

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

mkHasqlSettings :: AppConfig.DatabaseConfig -> Hasql.Settings
mkHasqlSettings dbConfig =
  settings
    (textToBS . AppConfig.getHost . AppConfig.host $ dbConfig)
    (fromIntegral . AppConfig.getPort . AppConfig.port $ dbConfig)
    (textToBS . AppConfig.getUser . AppConfig.user $ dbConfig)
    (textToBS . AppConfig.getPassword . AppConfig.password $ dbConfig)
    (textToBS . AppConfig.getDBName . AppConfig.dbname $ dbConfig)
  where
    textToBS = BS.pack . Text.unpack
