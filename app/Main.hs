{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import Api.AppServices (appServices)

-- base
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString.Char8 (unpack)

-- hasql
import Hasql.Connection (acquire)

-- servant-auth-server
import Servant.Auth.Server (generateKey)

-- warp
import Network.Wai.Handler.Warp (run)

main:: IO ()
main = do
  -- TODO: retrieve connection data from configuration file
  connection <- acquire "host=localhost port=5432 dbname=tagger user=user password=password"
  key <- generateKey
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> run 8080 $ app (appServices connection' key))
    connection
