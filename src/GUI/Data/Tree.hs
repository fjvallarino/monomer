{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module GUI.Data.Tree where

import Prelude hiding (lookup)

import qualified Data.Foldable as F
import qualified Data.Sequence as S

data Tree a = Node a (S.Seq (Tree a)) deriving (Functor, Foldable, Traversable)

type Path = [Int]

singleton :: a -> Tree a
singleton value = Node value S.empty

nodeValue :: Tree a -> a
nodeValue (Node value _) = value

nodeChildren :: Tree a -> S.Seq (Tree a)
nodeChildren (Node _ children) = children

nodeChildrenList :: Tree a -> [a]
nodeChildrenList (Node _ children) = seqToList children

seqToList :: (S.Seq (Tree a)) -> [a]
seqToList children = (fmap nodeValue . F.toList) children

fromList :: a -> [Tree a] -> Tree a
fromList value children = Node value (S.fromList children)

lookup :: Path -> Tree a -> Maybe a
lookup [] (Node val _) = Just val
lookup (idx:xs) (Node val seq) = case S.lookup idx seq of
  Just tree -> lookup xs tree
  otherwise -> Nothing

updateNode :: Path -> Tree a -> Tree a -> Tree a
updateNode [] _ new = new
updateNode (idx:xs) node@(Node val seq) new = case S.lookup idx seq of
  Just tree -> Node val newChildren where
    newChildren = S.update idx newNode seq
    newNode = updateNode xs tree new
  Nothing -> node
