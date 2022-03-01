{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Tagger.Content where

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- text
import Data.Text (Text)

data Content tag = Content
  { _content :: Text
  , _tags :: [tag]
  }
  deriving stock (Functor, Generic)

instance Foldable Content where
  foldMap f = foldMap f . _tags

instance Traversable Content where
  traverse f (Content content tags) = Content content <$> traverse f tags

instance ToSchema tag => ToSchema (Content tag)

instance FromJSON tag => FromJSON (Content tag)

instance ToJSON tag => ToJSON (Content tag)

hasAllTags :: Eq tag => [tag] -> Content tag -> Bool
hasAllTags tags content = and $ (\tag -> tag `elem` _tags content) <$> tags
