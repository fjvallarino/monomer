{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BookTypes where

import Control.Lens.TH
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer

data Book = Book {
  _bkTitle :: Text,
  _bkAuthors :: [Text],
  _bkYear :: Maybe Int,
  _bkCover :: Maybe Int
} deriving (Eq, Show)

instance FromJSON Book where
  parseJSON = withObject "Book" $ \b -> Book
    <$> b .: "title"
    <*> fmap (fromMaybe []) (b .:? "author_name")
    <*> b .:? "first_publish_year"
    <*> b .:? "cover_i"

data BookResp = BookResp {
  _brDocs :: [Book],
  _brFound :: Int
} deriving (Eq, Show)

instance FromJSON BookResp where
  parseJSON = withObject "BookResp" $ \b -> BookResp
    <$> b .: "docs"
    <*> b .: "numFound"

data BooksModel = BooksModel {
  _bkmQuery :: Text,
  _bmkSearching :: Bool,
  _bmkSelected :: Maybe Book,
  _bkmBooks :: [Book]
} deriving (Eq, Show, WidgetModel)

data BooksEvt
  = BooksInit
  | BooksSearch
  | BooksSearchResult BookResp
  | BooksSearchError Text
  | BooksShowDetails Book
  | BooksCloseDetails
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'Book
makeLensesWith abbreviatedFields 'BookResp
makeLensesWith abbreviatedFields 'BooksModel
