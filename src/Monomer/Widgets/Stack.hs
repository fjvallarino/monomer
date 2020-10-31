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
    nReqs = length vchildren
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    fixedW = fmap getFixedSize vreqsW
    fixedH = fmap getFixedSize vreqsH
    flexW = fmap (getFlexSize False) vreqsW
    flexH = fmap (getFlexSize False) vreqsH
    tmaxW = safeMaximum fixedW + safeMaximum flexW
    tmaxH = safeMaximum fixedH + safeMaximum flexH
    tsumW = sum fixedW + sum flexW
    tsumH = sum fixedH + sum flexH
    factW = getFactorMax vreqsW
    factH = getFactorMax vreqsH
    newSizeReqW
      | isVertical && Seq.null flexW = FixedSize (safeMaximum fixedW)
      | isVertical && Seq.null fixedW = FlexSize (safeMaximum flexW) factW
      | isVertical = rangeOrFixed (safeMaximum fixedW) tmaxW factW
      | Seq.null flexW = FixedSize (sum fixedW)
      | Seq.null fixedW = FlexSize (sum flexW) factW
      | otherwise = rangeOrFixed (sum fixedW) tsumW factW
    newSizeReqH
      | isHorizontal && Seq.null flexH = FixedSize (safeMaximum fixedH)
      | isHorizontal && Seq.null fixedH = FlexSize (safeMaximum flexH) factH
      | isHorizontal = rangeOrFixed (safeMaximum fixedH) tmaxH factH
      | Seq.null flexH = FixedSize (sum fixedH)
      | Seq.null fixedH = FlexSize (sum flexH) factH
      | otherwise = rangeOrFixed (sum fixedH) tsumH factH

  resize wenv viewport renderArea children inst = resized where
    style = activeStyle wenv inst
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    Rect x y w h = contentArea
    mainSize = if isHorizontal then w else h
    mainStart = if isHorizontal then x else y
    vchildren = Seq.filter _wiVisible children
    reqs = fmap mainReqSelector vchildren
    sSize = sum $ fmap getFixedSize reqs
    fSize = sum $ fmap (getFlexSize False) reqs
    fExists = fSize > 0
    fSizeFactor = sum $ fmap (getFlexSize True) reqs
    rSize = max 0 (mainSize - sSize)
    fExtra
      | fExists && fSize > 0 = (rSize - fSize) / fSizeFactor
      | otherwise = 0
    assignedArea = Seq.zip newViewports newViewports
    (newViewports, _) = foldl' foldHelper (Seq.empty, mainStart) children
    foldHelper (accum, offset) child = (newAccum, newOffset) where
      newSize = resizeChild contentArea fExtra offset child
      newAccum = accum |> newSize
      newOffset = offset + rectSelector newSize
    resized = (inst, assignedArea)

  resizeChild contentArea fExtra offset child = result where
    Rect l t w h = contentArea
    emptyRect = Rect l t 0 0
    mainSize = case mainReqSelector child of
      FixedSize sz -> sz
      FlexSize sz factor -> (1 + fExtra * factor) * sz
      RangeSize sz1 sz2 factor -> sz1 + (1 + fExtra * factor) * (sz2 - sz1)
    hRect = Rect offset t mainSize h
    vRect = Rect l offset w mainSize
    result
      | not $ _wiVisible child = emptyRect
      | isHorizontal = hRect
      | otherwise = vRect

  calcSize vchildren = calcSize_ vchildren False
  calcSizeFactor vchildren = calcSize_ vchildren True

  calcSize_ vchildren useFactor = Size width height where
    (maxW, sumW, maxH, sumH) = calcDimensions vchildren useFactor
    width
      | isHorizontal = sumW
      | otherwise = maxW
    height
      | isHorizontal = maxH
      | otherwise = sumH

  calcDimensions vchildren useFactor = (maxW, sumW, maxH, sumH) where
    getReqSize
      | useFactor = getReqFactored
      | otherwise = getMinSizeReq
    vreqsW = _wiSizeReqW <$> vchildren
    vreqsH = _wiSizeReqH <$> vchildren
    sumW = (sum . fmap getReqSize) vreqsW
    sumH = (sum . fmap getReqSize) vreqsH
    maxW = (safeMaximum . fmap getReqSize) vreqsW
    maxH = (safeMaximum . fmap getReqSize) vreqsH

  mainReqSelector
    | isHorizontal = _wiSizeReqW
    | otherwise = _wiSizeReqH

  sndReqSelector
    | isHorizontal = _wiSizeReqH
    | otherwise = _wiSizeReqW

  sizeSelector
    | isHorizontal = _sW
    | otherwise = _sH

  rectSelector
    | isHorizontal = _rW
    | otherwise = _rH

getFixedSize :: SizeReq -> Double
getFixedSize (FixedSize c) = c
getFixedSize (FlexSize c _) = 0
getFixedSize (RangeSize c1 _ _) = c1

getFlexSize :: Bool -> SizeReq -> Double
getFlexSize useFactor req = coord where
  factor
    | useFactor = getFactorReq req
    | otherwise = 1
  coord = case req of
    FixedSize c -> 0
    FlexSize c _ -> c * factor
    RangeSize c1 c2 _ -> (c2 - c1) * factor

getFactorMax :: Seq SizeReq -> Double
getFactorMax reqs
  | Seq.null flexReqs = 1
  | otherwise = maximum (fmap getFactorReq flexReqs)
  where
    flexReqs = Seq.filter (not . isFixedSizeReq) reqs

getFactorAvg :: Seq SizeReq -> Double
getFactorAvg reqs
  | Seq.null flexReqs = 1
  | otherwise = sum (fmap getFactorReq flexReqs) / flexCount
  where
    flexReqs = Seq.filter (not . isFixedSizeReq) reqs
    flexCount = fromIntegral (Seq.length flexReqs)

getReqFactored :: SizeReq -> Double
getReqFactored req = getFactorReq req * getMinSizeReq req

rangeOrFixed :: Double -> Double -> Factor -> SizeReq
rangeOrFixed val1 val2 factor
  | abs (val2 - val1) < 0.01 = FixedSize val1
  | otherwise = RangeSize val1 val2 factor

safeMaximum Empty = 0
safeMaximum xs = maximum xs
