module Monomer.Widgets.Stack (
  hstack,
  hstack_,
  vstack,
  vstack_
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

newtype StackCfg = StackCfg {
  _stcIgnoreEmptyClick :: Maybe Bool
}

instance Default StackCfg where
  def = StackCfg Nothing

instance Semigroup StackCfg where
  (<>) s1 s2 = StackCfg {
    _stcIgnoreEmptyClick = _stcIgnoreEmptyClick s2 <|> _stcIgnoreEmptyClick s1
  }

instance Monoid StackCfg where
  mempty = def

instance IgnoreEmptyClick StackCfg where
  ignoreEmptyClick ignore = def {
    _stcIgnoreEmptyClick = Just ignore
  }

hstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
hstack children = hstack_ children def

hstack_
  :: (Traversable t)
  => t (WidgetInstance s e)
  -> [StackCfg]
  -> WidgetInstance s e
hstack_ children configs = newInst where
  config = mconcat configs
  newInst = (defaultWidgetInstance "hstack" (makeStack True config)) {
  _wiChildren = foldl' (|>) Empty children
}

vstack :: (Traversable t) => t (WidgetInstance s e) -> WidgetInstance s e
vstack children = vstack_ children def

vstack_
  :: (Traversable t)
  => t (WidgetInstance s e)
  -> [StackCfg]
  -> WidgetInstance s e
vstack_ children configs = newInst where
  config = mconcat configs
  newInst = (defaultWidgetInstance "vstack" (makeStack False config)) {
  _wiChildren = foldl' (|>) Empty children
}

makeStack :: Bool -> StackCfg -> Widget s e
makeStack isHorizontal config = widget where
  baseWidget = createContainer def {
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetFindByPoint = findByPointWrapper ignoreEmptyClick defaultFindByPoint
  }

  ignoreEmptyClick = _stcIgnoreEmptyClick config == Just True
  isVertical = not isHorizontal

  getSizeReq wenv inst children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter _wiVisible children
    newSizeReqW = getDimSizeReq isHorizontal _wiSizeReqW vchildren
    newSizeReqH = getDimSizeReq isVertical _wiSizeReqH vchildren

  getDimSizeReq mainAxis accesor vchildren
    | Seq.null vreqs = FixedSize 0
    | mainAxis = foldl1 mergeSizeReqSum vreqs
    | otherwise = foldl1 mergeSizeReqMax vreqs
    where
      vreqs = accesor <$> vchildren

  resize wenv viewport renderArea children inst = resized where
    style = activeStyle wenv inst
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect x y w h = contentArea
    mainSize = if isHorizontal then w else h
    mainStart = if isHorizontal then x else y
    vchildren = Seq.filter _wiVisible children
    reqs = fmap mainReqSelector vchildren
    sumSizes accum req = newStep where
      (cFixed, cFlex, cFlexFac, cExtraFac) = accum
      newFixed = cFixed + sizeFixed req
      newFlex = cFlex + sizeFlex req
      newFlexFac = cFlexFac + sizeFlex req * sizeFactor req
      newExtraFac = cExtraFac + sizeExtra req * sizeFactor req
      newStep = (newFixed, newFlex, newFlexFac, newExtraFac)
    (fixed, flex, flexFac, extraFac) = foldl' sumSizes def reqs
    flexAvail = max 0 $ min flex (mainSize - fixed)
    extraAvail = max 0 (mainSize - fixed - flex)
    flexCoeff
      | flexFac > 0 = flexAvail / flexFac
      | otherwise = 0
    extraCoeff
      | extraFac > 0 = extraAvail / extraFac
      | otherwise = 0
    foldHelper (accum, offset) child = (newAccum, newOffset) where
      newSize = resizeChild contentArea flexCoeff extraCoeff offset child
      newAccum = accum |> newSize
      newOffset = offset + rectSelector newSize
    (newViewports, _) = foldl' foldHelper (Seq.empty, mainStart) children
    assignedArea = Seq.zip newViewports newViewports
    resized = (inst, assignedArea)

  resizeChild contentArea flexCoeff extraCoeff offset child = result where
    Rect l t w h = contentArea
    emptyRect = Rect l t 0 0
    totalCoeff = flexCoeff + extraCoeff
    mainSize = case mainReqSelector child of
      FixedSize sz -> sz
      FlexSize sz factor -> (totalCoeff * factor) * sz
      MinSize sz factor -> (1 + totalCoeff * factor) * sz
      MaxSize sz factor -> flexCoeff * factor * sz
      RangeSize sz1 sz2 factor -> sz1 + flexCoeff * factor * (sz2 - sz1)
    hRect = Rect offset t mainSize h
    vRect = Rect l offset w mainSize
    result
      | not $ _wiVisible child = emptyRect
      | isHorizontal = hRect
      | otherwise = vRect

  mainReqSelector
    | isHorizontal = _wiSizeReqW
    | otherwise = _wiSizeReqH

  rectSelector
    | isHorizontal = _rW
    | otherwise = _rH

sizeFixed :: SizeReq -> Double
sizeFixed (FixedSize s) = s
sizeFixed (FlexSize s _) = 0
sizeFixed (MinSize s _) = s
sizeFixed (MaxSize s _) = 0
sizeFixed (RangeSize s1 _ _) = s1

sizeFlex :: SizeReq -> Double
sizeFlex (FixedSize s) = 0
sizeFlex (FlexSize s _) = s
sizeFlex (MinSize s _) = s
sizeFlex (MaxSize s _) = s
sizeFlex (RangeSize s1 s2 _) = s2 - s1

sizeExtra :: SizeReq -> Double
sizeExtra (FlexSize s _) = s
sizeExtra (MinSize s _) = s
sizeExtra _ = 0

sizeFactor :: SizeReq -> Double
sizeFactor (FixedSize _) = 1
sizeFactor (FlexSize _ f) = f
sizeFactor (MinSize _ f) = f
sizeFactor (MaxSize _ f) = f
sizeFactor (RangeSize _ _ f) = f
