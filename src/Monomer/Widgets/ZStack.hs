{-# LANGUAGE FlexibleContexts #-}

module Monomer.Widgets.ZStack (
  zstack,
  zstack_,
  onlyTopActive
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
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

zstack
  :: (WidgetModel s, WidgetEvent e, Traversable t)
  => t (WidgetNode s e)
  -> WidgetNode s e
zstack children = zstack_ children def

zstack_
  :: (WidgetModel s, WidgetEvent e, Traversable t)
  => t (WidgetNode s e)
  -> [ZStackCfg]
  -> WidgetNode s e
zstack_ children configs = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "zstack" (makeZStack config)
    & L.children .~ Seq.reverse (foldl' (|>) Empty children)

makeZStack :: (WidgetModel s, WidgetEvent e) => ZStackCfg -> Widget s e
makeZStack config = widget where
  baseWidget = createContainer def {
    containerKeepChildrenSizes = True,
    containerFindNextFocus = findNextFocus,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetFindByPoint = findByPoint,
    widgetRender = render
  }

  -- | Find instance matching point
  findByPoint wenv startPath point node = result where
    onlyTop = fromMaybe True (_zscOnlyTopActive config)
    children = node ^. L.children
    vchildren
      | onlyTop = Seq.take 1 $ Seq.filter (_wiVisible . _wnWidgetInstance) children
      | otherwise = Seq.filter (_wiVisible . _wnWidgetInstance) children
    newStartPath = Seq.drop 1 startPath
    result = findFirstByPoint vchildren wenv newStartPath point

  findNextFocus wenv direction start node = result where
    onlyTop = fromMaybe True (_zscOnlyTopActive config)
    children = node ^. L.children
    vchildren = Seq.filter (_wiVisible . _wnWidgetInstance) children
    result
      | onlyTop = Seq.take 1 vchildren
      | otherwise = vchildren

  getSizeReq wenv node children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter (_wiVisible . _wnWidgetInstance) children
    newSizeReqW = getDimSizeReq (_wiSizeReqW . _wnWidgetInstance) vchildren
    newSizeReqH = getDimSizeReq (_wiSizeReqH . _wnWidgetInstance) vchildren

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
      viewport = node ^. L.widgetInstance . L.viewport
      renderArea = node ^. L.widgetInstance . L.renderArea
      isVisible c = c ^. L.widgetInstance . L.visible
      topVisibleIdx = fromMaybe 0 (Seq.findIndexR (_wiVisible . _wnWidgetInstance) children)
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
  isVisible = ch ^. L.widgetInstance . L.visible
  newPath = widgetFindByPoint (ch ^. L.widget) wenv startPath point ch
  result
    | isVisible && isJust newPath = newPath
    | otherwise = findFirstByPoint chs wenv startPath point
