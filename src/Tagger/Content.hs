{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tagger.Content where

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- text
import Data.Text (Text)

-- |
-- A 'Content' is just a text indexed by a list of 'tag's
data Content tag = Content
  { _message :: Text
  , _tags :: [tag]
  }
  deriving stock (Eq, Show, Functor, Generic)

instance Foldable Content where
  foldMap f = foldMap f . _tags

instance Traversable Content where
  traverse f Content{_message, _tags} = Content _message <$> traverse f _tags

instance ToSchema tag => ToSchema (Content tag)

instance FromJSON tag => FromJSON (Content tag)

instance ToJSON tag => ToJSON (Content tag)

-- |
-- checks whether a 'Content' is indexed by all the provided 'tag's
hasAllTags :: Eq tag => [tag] -> Content tag -> Bool
hasAllTags tags content = and $ (\tag -> tag `elem` _tags content) <$> tags
