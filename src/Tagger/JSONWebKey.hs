{-# LANGUAGE ScopedTypeVariables #-}

module Tagger.JSONWebKey (setup, JWK) where

import Control.Exception (catch)
import Crypto.JOSE.JWK (JWK)
import Data.ByteString.Char8 (writeFile)
import Servant.Auth.Server (fromSecret, generateSecret, readKey)
import Prelude hiding (writeFile)

setup :: FilePath -> IO JWK
setup path = do
  -- try to retrieve the JWK from file
  catch (readKey path) $ \(_ :: IOError) -> do
    -- if the file does not exist or does not contain a valid key, we generate one
    key <- generateSecret
    -- and we store it
    writeFile path key
    pure $ fromSecret key
