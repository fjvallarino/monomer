{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Monomer.Common.Tree where

import Prelude hiding (lookup)

import Data.Sequence (Seq(..), (<|), (|>), (><))
import qualified Data.Foldable as F

import qualified Data.Sequence as Seq

type PathStep = Int
type Path = Seq PathStep
type Nodes a = Seq (Tree a)

data Tree a = Node a (Nodes a) deriving (Functor, Foldable, Traversable)

singleton :: a -> Tree a
singleton value = Node value Seq.empty

nodeValue :: Tree a -> a
nodeValue (Node value _) = value

nodeChildren :: Tree a -> Seq.Seq (Tree a)
nodeChildren (Node _ children) = children

nodeChildrenList :: Tree a -> [a]
nodeChildrenList (Node _ children) = seqToList children

seqToList :: (Seq.Seq (Tree a)) -> [a]
seqToList children = (fmap nodeValue . F.toList) children

seqToNodeList :: (Seq.Seq (Tree a)) -> [Tree a]
seqToNodeList children = F.toList children

fromList :: a -> [Tree a] -> Tree a
fromList value children = Node value (Seq.fromList children)

lookup :: Path -> Tree a -> Maybe a
lookup Empty (Node val _) = Just val
lookup (idx :<| xs) (Node val seq) = case Seq.lookup idx seq of
  Just tree -> lookup xs tree
  otherwise -> Nothing

updateNode :: Path -> Tree a -> (Tree a -> Tree a) -> Maybe (Tree a)
updateNode Empty old updateFn = Just $ updateFn old
updateNode (idx :<| xs) node@(Node val seq) updateFn = case Seq.lookup idx seq of
  Just tree -> case updateNode xs tree updateFn of
    Just newNode -> Just $ Node val (Seq.update idx newNode seq) where
    Nothing -> Nothing
  Nothing -> Nothing

replaceNode :: Path -> Tree a -> Tree a -> Maybe (Tree a)
replaceNode path root new = updateNode path root (const new)
