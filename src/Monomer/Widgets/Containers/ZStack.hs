{-|
Module      : Monomer.Widgets.Containers.ZStack
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Layered container, stacking children one on top of the other. Useful for
handling widgets that need to be visible in certain contexts only (dialogs), or
to overlay unrelated widgets (text on top of an image).

The order of the widgets is from bottom to top.

The container will request the largest horizontal and vertical size from its
child nodes.

Config:

- onlyTopActive: whether the top visible node is the only node that may receive
events.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.Containers.ZStack (
  zstack,
  zstack_,
  onlyTopActive
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (^?), (.~), (%~), (?~), at, ix)
import Control.Monad (forM_, void, when)
import Data.Default
import Data.Maybe
import Data.List (foldl', any)
import Data.Sequence (Seq(..), (<|), (|>))
import GHC.Generics

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype ZStackCfg = ZStackCfg {
  _zscOnlyTopActive :: Maybe Bool
}

instance Default ZStackCfg where
  def = ZStackCfg Nothing

instance Semigroup ZStackCfg where
  (<>) z1 z2 = ZStackCfg {
    _zscOnlyTopActive = _zscOnlyTopActive z2 <|> _zscOnlyTopActive z1
  }

instance Monoid ZStackCfg where
  mempty = def

-- | Whether the top visible node is the only node that may receive events.
-- | Defaults to True.
onlyTopActive :: Bool -> ZStackCfg
onlyTopActive active = def {
  _zscOnlyTopActive = Just active
}

data ZStackState = ZStackState {
  _zssFocusMap :: M.Map PathStep WidgetId,
  _zssTopIdx :: Int
} deriving (Eq, Show, Generic)

-- | Creates a zstack container with the provided nodes.
zstack :: (Traversable t) => t (WidgetNode s e) -> WidgetNode s e
zstack children = zstack_ def children

-- | Creates a zstack container with the provided nodes. Accepts config.
zstack_
  :: (Traversable t)
  => [ZStackCfg]
  -> t (WidgetNode s e)
  -> WidgetNode s e
zstack_ configs children = newNode where
  config = mconcat configs
  state = ZStackState M.empty 0
  newNode = defaultWidgetNode "zstack" (makeZStack config state)
    & L.children .~ Seq.reverse (foldl' (|>) Empty children)

makeZStack :: ZStackCfg -> ZStackState -> Widget s e
makeZStack config state = widget where
  baseWidget = createContainer state def {
    containerUseChildrenSizes = True,
    containerInit = init,
    containerMergePost = mergePost,
    containerFindNextFocus = findNextFocus,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetFindByPoint = findByPoint,
    widgetRender = render
  }

  onlyTopActive = fromMaybe True (_zscOnlyTopActive config)

  init wenv node = resultNode newNode where
    children = node ^. L.children
    focusedPath = wenv ^. L.focusedPath
    newState = state {
      _zssTopIdx = fromMaybe 0 (Seq.findIndexL (^.L.info . L.visible) children)
    }
    newNode = node
      & L.widget .~ makeZStack config newState

  mergePost wenv node oldNode oldState newState result = newResult where
    ZStackState oldFocusMap oldTopIdx = oldState
    children = node ^. L.children
    focusedPath = wenv ^. L.focusedPath
    focusedWid = findWidgetIdFromPath wenv focusedPath
    isFocusParent = isNodeParentOfPath node focusedPath

    topLevel = isNodeTopLevel wenv node
    flagsChanged = childrenFlagsChanged oldNode node
    newTopIdx = fromMaybe 0 (Seq.findIndexL (^.L.info . L.visible) children)
    focusReq = isJust $ Seq.findIndexL isFocusRequest (result ^. L.requests)
    needsFocus = isFocusParent && topLevel && flagsChanged && not focusReq

    oldTopWid = M.lookup newTopIdx oldFocusMap
    fstTopWid = node ^? L.children . ix newTopIdx . L.info . L.widgetId
    newState = oldState {
      _zssFocusMap = oldFocusMap & at oldTopIdx .~ focusedWid,
      _zssTopIdx = newTopIdx
    }

    tmpResult = result
      & L.node . L.widget .~ makeZStack config newState
    newResult
      | needsFocus && isJust oldTopWid = tmpResult
          & L.requests %~ (|> SetFocus (fromJust oldTopWid))
      | needsFocus = tmpResult
          & L.requests %~ (|> MoveFocus fstTopWid FocusFwd)
      | isFocusParent = tmpResult
      | otherwise = result

  -- | Find instance matching point
  findByPoint wenv node start point = result where
    children = node ^. L.children
    vchildren
      | onlyTopActive = Seq.take 1 $ Seq.filter (_wniVisible . _wnInfo) children
      | otherwise = Seq.filter (_wniVisible . _wnInfo) children

    nextStep = nextTargetStep node start
    ch = Seq.index children (fromJust nextStep)
    visible = node ^. L.info . L.visible
    childVisible = ch ^. L.info . L.visible
    isNextValid = isJust nextStep && visible && childVisible
    result
      | isNextValid = widgetFindByPoint (ch ^. L.widget) wenv ch start point
      | visible = findFirstByPoint vchildren wenv start point
      | otherwise = Nothing

  findNextFocus wenv node direction start = result where
    children = node ^. L.children
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    result
      | onlyTopActive = Seq.take 1 vchildren
      | otherwise = vchildren

  getSizeReq wenv node children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    newSizeReqW = getDimSizeReq (_wniSizeReqW . _wnInfo) vchildren
    newSizeReqH = getDimSizeReq (_wniSizeReqH . _wnInfo) vchildren

  getDimSizeReq accesor vchildren
    | Seq.null vreqs = fixedSize 0
    | otherwise = foldl1 sizeReqMergeMax vreqs
    where
      vreqs = accesor <$> vchildren

  resize wenv node viewport children = resized where
    style = currentStyle wenv node
    vpChild = fromMaybe def (removeOuterBounds style viewport)
    assignedAreas = fmap (const vpChild) children
    resized = (resultNode node, assignedAreas)

  render wenv node renderer =
    drawInScissor renderer True viewport $
      drawStyledAction renderer viewport style $ \_ ->
        void $ Seq.traverseWithIndex renderChild children
    where
      style = currentStyle wenv node
      children = Seq.reverse $ node ^. L.children
      viewport = node ^. L.info . L.viewport
      isVisible c = c ^. L.info . L.visible
      topVisibleIdx = fromMaybe 0 (Seq.findIndexR (_wniVisible . _wnInfo) children)

      isPointEmpty point idx = not covered where
        prevs = Seq.drop (idx + 1) children
        target c = widgetFindByPoint (c ^. L.widget) wenv c emptyPath point
        isCovered c = isVisible c && isJust (target c)
        covered = any isCovered prevs

      isTopLayer idx child point = prevTopLayer && isValid where
        prevTopLayer = _weInTopLayer wenv point
        isValid
          | onlyTopActive = idx == topVisibleIdx
          | otherwise = isPointEmpty point idx

      cWenv idx child = wenv {
        _weInTopLayer = isTopLayer idx child
      }
      renderChild idx child = when (isVisible child) $
        widgetRender (child ^. L.widget) (cWenv idx child) child renderer

findFirstByPoint
  :: Seq (WidgetNode s e)
  -> WidgetEnv s e
  -> Seq PathStep
  -> Point
  -> Maybe WidgetNodeInfo
findFirstByPoint Empty _ _ _ = Nothing
findFirstByPoint (ch :<| chs) wenv start point = result where
  isVisible = ch ^. L.info . L.visible
  newPath = widgetFindByPoint (ch ^. L.widget) wenv ch start point
  result
    | isVisible && isJust newPath = newPath
    | otherwise = findFirstByPoint chs wenv start point
