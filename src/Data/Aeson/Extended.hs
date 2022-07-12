module Data.Aeson.Extended
  ( module Data.Aeson,
    asText,
    fromKV,
  )
where

import Data.Aeson (Encoding, fromEncoding, pairs, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.ByteString.Builder as BS (toLazyByteString)
import qualified Data.ByteString.Lazy as BS (toStrict)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

asText :: Encoding -> Text
asText = T.decodeUtf8 . BS.toStrict . BS.toLazyByteString . fromEncoding

fromKV :: [(Text, Text)] -> Encoding
fromKV = pairs . foldMap (\(k, v) -> fromText k .= v)
