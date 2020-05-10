{-# LANGUAGE FlexibleContexts #-}

module Monomer.Main.Util where

import Lens.Micro.Mtl

import Monomer.Common.Core
import Monomer.Data.Tree
import Monomer.Event.Types

getCurrentFocus :: (MonomerM s e m) => m Path
getCurrentFocus = do
  ring <- use focusRing
  return (if length ring > 0 then ring!!0 else [])

collectPaths :: (Monad m) => Tree (WidgetInstance s e m) -> Path -> [(WidgetInstance s e m, Path)]
collectPaths treeNode path = fmap (\(node, path) -> (node, reverse path)) (collectReversedPaths treeNode path)

collectReversedPaths :: (Monad m) => Tree (WidgetInstance s e m) -> Path -> [(WidgetInstance s e m, Path)]
collectReversedPaths (Node widgetNode children) path = (widgetNode, path) : remainingItems where
  pairs = zip (seqToNodeList children) (map (: path) [0..])
  remainingItems = concatMap (\(wn, path) -> collectReversedPaths wn path) pairs

isCustomHandler :: (Path, EventRequest) -> Bool
isCustomHandler (_, RunCustom _) = True
isCustomHandler _ = False

isUpdateUserState :: (Path, EventRequest) -> Bool
isUpdateUserState (_, UpdateUserState) = True
isUpdateUserState _ = False
