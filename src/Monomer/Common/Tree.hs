{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Monomer.Common.Tree where

import Prelude hiding (lookup)

import Data.Sequence (Seq(..))

import qualified Data.Sequence as Seq

type PathStep = Int
type Path = Seq PathStep

data Tree a = Node {
  nodeValue :: a,
  nodeChildren :: Seq (Tree a)
} deriving (Functor, Foldable, Traversable)

singleNode :: a -> Tree a
singleNode value = Node value Seq.empty

lookup :: Path -> Tree a -> Maybe a
lookup Empty (Node val _) = Just val
lookup (idx :<| xs) (Node val seq) = Seq.lookup idx seq >>= lookup xs

updateNode :: Path -> Tree a -> (Tree a -> Tree a) -> Maybe (Tree a)
updateNode Empty old updateFn = Just $ updateFn old
updateNode (idx :<| xs) node@(Node val seq) updateFn = do
  tree <- Seq.lookup idx seq
  newNode <- updateNode xs tree updateFn
  return $ Node val (Seq.update idx newNode seq)

replaceNode :: Path -> Tree a -> Tree a -> Maybe (Tree a)
replaceNode path root new = updateNode path root (const new)
