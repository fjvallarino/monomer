module Monomer.Widgets.ZStack (
  zstack,
  zstack_,
  onlyTopFocusable
) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

newtype ZStackCfg = ZStackCfg {
  _zscOnlyTopFocusable :: Maybe Bool
}

instance Default ZStackCfg where
  def = ZStackCfg Nothing

instance Semigroup ZStackCfg where
  (<>) z1 z2 = ZStackCfg {
    _zscOnlyTopFocusable = _zscOnlyTopFocusable z2 <|> _zscOnlyTopFocusable z1
  }

instance Monoid ZStackCfg where
  mempty = def

onlyTopFocusable :: Bool -> ZStackCfg
onlyTopFocusable onlyTop = def {
  _zscOnlyTopFocusable = Just onlyTop
}

zstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
zstack children = zstack_ children def

zstack_
  :: (Traversable t)
  => t (WidgetInstance s e)
  -> [ZStackCfg]
  -> WidgetInstance s e
zstack_ children configs = newInst where
  config = mconcat configs
  newInst = (defaultWidgetInstance "zstack" (makeZStack config)) {
  _wiChildren = Seq.reverse $ foldl' (|>) Empty children
}

makeZStack :: ZStackCfg -> Widget s e
makeZStack config = widget where
  baseWidget = createContainer def {
    containerFindNextFocus = findNextFocus,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetFindByPoint = findByPoint,
    widgetRender = render
  }

  -- | Find instance matching point
  findByPoint wenv startPath point inst = result where
    children = _wiChildren inst
    result = findFirstByPoint children wenv startPath point

  findNextFocus wenv direction start inst = result where
    onlyTop = fromMaybe True (_zscOnlyTopFocusable config)
    children = _wiChildren inst
    vchildren = Seq.filter _wiVisible children
    result
      | onlyTop = Seq.take 1 vchildren
      | otherwise = vchildren

  getSizeReq wenv inst children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter _wiVisible children
    nReqs = length vchildren
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    fixedReqs reqs = Seq.filter isFixedSizeReq reqs
    fixedW = nReqs > 0 && Seq.length (fixedReqs vreqsW) == nReqs
    fixedH = nReqs > 0 && Seq.length (fixedReqs vreqsH) == nReqs
    factor = 1
    width
      | Seq.null vreqsW = 0
      | otherwise = maximum $ fmap getMaxSizeReq vreqsW
    height
      | Seq.null vreqsH = 0
      | otherwise = maximum $ fmap getMaxSizeReq vreqsH
    newSizeReqW
      | fixedW = FixedSize width
      | otherwise = FlexSize width factor
    newSizeReqH
      | fixedH = FixedSize height
      | otherwise = FlexSize height factor

  resize wenv viewport renderArea children inst = resized where
    assignedAreas = fmap (const (viewport, renderArea)) children
    resized = (inst, assignedAreas)

  render renderer wenv inst =
    drawInScissor renderer True viewport $
      drawStyledAction renderer renderArea style $ \_ ->
        forM_ children $ \child -> when (isVisible child) $
          widgetRender (_wiWidget child) renderer wenv child
    where
      style = activeStyle wenv inst
      children = Seq.reverse $ _wiChildren inst
      viewport = _wiViewport inst
      renderArea = _wiRenderArea inst
      isVisible c = _wiVisible c

findFirstByPoint
  :: Seq (WidgetInstance s e)
  -> WidgetEnv s e
  -> Seq PathStep
  -> Point
  -> Maybe Path
findFirstByPoint Empty _ _ _ = Nothing
findFirstByPoint (ch :<| chs) wenv startPath point = result where
  isVisible = _wiVisible ch
  newPath = widgetFindByPoint (_wiWidget ch) wenv startPath point ch
  result
    | isVisible && isJust newPath = newPath
    | otherwise = findFirstByPoint chs wenv startPath point
