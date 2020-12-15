{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.ZStack (
  zstack,
  zstack_,
  onlyTopActive
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Monad (forM_, void, when)
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|), (|>))

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

zstack :: (Traversable t) => t (WidgetNode s e) -> WidgetNode s e
zstack children = zstack_ children def

zstack_
  :: (Traversable t)
  => t (WidgetNode s e)
  -> [ZStackCfg]
  -> WidgetNode s e
zstack_ children configs = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "zstack" (makeZStack config)
    & L.children .~ Seq.reverse (foldl' (|>) Empty children)

makeZStack :: ZStackCfg -> Widget s e
makeZStack config = widget where
  baseWidget = createContainer def {
    containerKeepChildrenSizes = True,
    containerMergePost = mergePost,
    containerFindNextFocus = findNextFocus,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetFindByPoint = findByPoint,
    widgetRender = render
  }

  mergePost wenv result oldState oldNode newNode = newResult where
    children = newNode ^. L.children
    focusedPath = wenv ^. L.focusedPath
    isFocusParent = isWidgetParentOfPath focusedPath newNode
    topLevel = isTopLevel wenv newNode
    childrenChanged = visibleChildrenChanged oldNode newNode
    topVisibleIdx = fromMaybe 0 (Seq.findIndexL (^.L.info . L.visible) children)
    needsFocus = isFocusParent && topLevel && childrenChanged
    newPath = Just $ newNode ^. L.info . L.path |> topVisibleIdx

    newResult
      | needsFocus = result & L.requests %~ (|> MoveFocus newPath FocusFwd)
      | otherwise = result

  -- | Find instance matching point
  findByPoint wenv startPath point node = result where
    onlyTop = fromMaybe True (_zscOnlyTopActive config)
    children = node ^. L.children
    vchildren
      | onlyTop = Seq.take 1 $ Seq.filter (_wniVisible . _wnInfo) children
      | otherwise = Seq.filter (_wniVisible . _wnInfo) children
    newStartPath = Seq.drop 1 startPath
    result = findFirstByPoint vchildren wenv newStartPath point

  findNextFocus wenv direction start node = result where
    onlyTop = fromMaybe True (_zscOnlyTopActive config)
    children = node ^. L.children
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    result
      | onlyTop = Seq.take 1 vchildren
      | otherwise = vchildren

  getSizeReq wenv node children = (newSizeReqW, newSizeReqH) where
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
    resized = (node, assignedAreas)

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
      isTopLayer idx child point = prevTopLayer && isTopChild where
        isTopChild = idx == topVisibleIdx
        prevTopLayer = _weInTopLayer wenv point
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
