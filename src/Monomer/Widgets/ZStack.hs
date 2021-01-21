{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.ZStack (
  zstack,
  zstack_,
  onlyTopActive
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), (?~), at)
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

onlyTopActive :: Bool -> ZStackCfg
onlyTopActive active = def {
  _zscOnlyTopActive = Just active
}

data ZStackState = ZStackState {
  _zssFocusMap :: M.Map PathStep Path,
  _zssTopIdx :: Int
} deriving (Eq, Show, Generic, Serialise)

zstack :: (Traversable t) => t (WidgetNode s e) -> WidgetNode s e
zstack children = zstack_ children def

zstack_
  :: (Traversable t)
  => t (WidgetNode s e)
  -> [ZStackCfg]
  -> WidgetNode s e
zstack_ children configs = newNode where
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

  init wenv node = resultWidget newNode where
    children = node ^. L.children
    focusedPath = wenv ^. L.focusedPath
    newState = state {
      _zssTopIdx = fromMaybe 0 (Seq.findIndexL (^.L.info . L.visible) children)
    }
    newNode = node
      & L.widget .~ makeZStack config newState

  mergePost wenv result oldState oldNode node = newResult where
    ZStackState oldFocusMap oldTopIdx = oldState
    children = node ^. L.children
    focusedPath = wenv ^. L.focusedPath
    isFocusParent = isWidgetParentOfPath focusedPath node
    topLevel = isNodeTopLevel wenv node
    childrenChanged = visibleChildrenChanged oldNode node
    newTopIdx = fromMaybe 0 (Seq.findIndexL (^.L.info . L.visible) children)
    focusReq = isJust $ Seq.findIndexL isFocusRequest (result ^. L.requests)
    needsFocus = isFocusParent && topLevel && childrenChanged && not focusReq
    oldFocus = fromJust oldTopPath
    oldTopPath = M.lookup newTopIdx oldFocusMap
    fstTopPath = Just $ node ^. L.info . L.path |> newTopIdx
    newState = oldState {
      _zssFocusMap = oldFocusMap & at oldTopIdx ?~ focusedPath,
      _zssTopIdx = newTopIdx
    }
    tmpResult = result
      & L.node . L.widget .~ makeZStack config newState
    newResult
      | needsFocus && isJust oldTopPath = tmpResult
          & L.requests %~ (|> SetFocus (fromJust oldTopPath))
      | needsFocus = tmpResult
          & L.requests %~ (|> MoveFocus fstTopPath FocusFwd)
      | isFocusParent = tmpResult
      | otherwise = result

  -- | Find instance matching point
  findByPoint wenv startPath point node = result where
    children = node ^. L.children
    vchildren
      | onlyTopActive = Seq.take 1 $ Seq.filter (_wniVisible . _wnInfo) children
      | otherwise = Seq.filter (_wniVisible . _wnInfo) children
    newStartPath = Seq.drop 1 startPath
    result = findFirstByPoint vchildren wenv newStartPath point

  findNextFocus wenv direction start node = result where
    children = node ^. L.children
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    result
      | onlyTopActive = Seq.take 1 vchildren
      | otherwise = vchildren

  getSizeReq wenv currState node children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    newSizeReqW = getDimSizeReq (_wniSizeReqW . _wnInfo) vchildren
    newSizeReqH = getDimSizeReq (_wniSizeReqH . _wnInfo) vchildren

  getDimSizeReq accesor vchildren
    | Seq.null vreqs = FixedSize 0
    | otherwise = foldl1 sizeReqMergeMax vreqs
    where
      vreqs = accesor <$> vchildren

  resize wenv viewport renderArea children node = resized where
    style = activeStyle wenv node
    raChild = fromMaybe def (removeOuterBounds style renderArea)
    vpChild = fromMaybe def (intersectRects viewport raChild)
    assignedAreas = fmap (const (vpChild, raChild)) children
    resized = (resultWidget node, assignedAreas)

  render renderer wenv node =
    drawInScissor renderer True viewport $
      drawStyledAction renderer renderArea style $ \_ ->
        void $ Seq.traverseWithIndex renderChild children
    where
      style = activeStyle wenv node
      children = Seq.reverse $ node ^. L.children
      viewport = node ^. L.info . L.viewport
      renderArea = node ^. L.info . L.renderArea
      isVisible c = c ^. L.info . L.visible
      topVisibleIdx = fromMaybe 0 (Seq.findIndexR (_wniVisible . _wnInfo) children)
      isPointEmpty point idx = not covered where
        prevs = Seq.drop (idx + 1) children
        target c = widgetFindByPoint (c ^. L.widget) wenv Empty point c
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
        widgetRender (child ^. L.widget) renderer (cWenv idx child) child

findFirstByPoint
  :: Seq (WidgetNode s e)
  -> WidgetEnv s e
  -> Seq PathStep
  -> Point
  -> Maybe Path
findFirstByPoint Empty _ _ _ = Nothing
findFirstByPoint (ch :<| chs) wenv startPath point = result where
  isVisible = ch ^. L.info . L.visible
  newPath = widgetFindByPoint (ch ^. L.widget) wenv startPath point ch
  result
    | isVisible && isJust newPath = newPath
    | otherwise = findFirstByPoint chs wenv startPath point
