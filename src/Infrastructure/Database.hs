{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database (acquire) where

import qualified Api.Config as AppConfig
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Hasql.Connection as Hasql
import Hasql.Connection (Connection)

acquire :: AppConfig.Config -> IO Connection
acquire config = do
  eConn <- Hasql.acquire . AppConfig.connectionString . AppConfig.database $ config
  either
    (fail . BS.unpack . fromMaybe "unable to connect to the database")
    pure
    eConn
