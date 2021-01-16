module Monomer.Widgets.Stack (
  hstack,
  hstack_,
  vstack,
  vstack_,
  assignStackAreas
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype StackCfg = StackCfg {
  _stcIgnoreEmptyArea :: Maybe Bool
}

instance Default StackCfg where
  def = StackCfg Nothing

instance Semigroup StackCfg where
  (<>) s1 s2 = StackCfg {
    _stcIgnoreEmptyArea = _stcIgnoreEmptyArea s2 <|> _stcIgnoreEmptyArea s1
  }

instance Monoid StackCfg where
  mempty = def

instance CmbIgnoreEmptyArea StackCfg where
  ignoreEmptyArea ignore = def {
    _stcIgnoreEmptyArea = Just ignore
  }

hstack :: (Traversable t) => t (WidgetNode s e) -> WidgetNode s e
hstack children = hstack_ children def

hstack_
  :: (Traversable t)
  => t (WidgetNode s e)
  -> [StackCfg]
  -> WidgetNode s e
hstack_ children configs = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "hstack" (makeStack True config)
    & L.children .~ foldl' (|>) Empty children

vstack :: (Traversable t) => t (WidgetNode s e) -> WidgetNode s e
vstack children = vstack_ children def

vstack_
  :: (Traversable t)
  => t (WidgetNode s e)
  -> [StackCfg]
  -> WidgetNode s e
vstack_ children configs = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "vstack" (makeStack False config)
    & L.children .~ foldl' (|>) Empty children

makeStack :: Bool -> StackCfg -> Widget s e
makeStack isHorizontal config = widget where
  widget = createContainer () def {
    containerIgnoreEmptyArea = ignoreEmptyArea,
    containerUseCustomSize = True,
    containerFindByPoint = defaultFindByPoint,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  isVertical = not isHorizontal
  ignoreEmptyArea = fromMaybe False (_stcIgnoreEmptyArea config)

  getSizeReq wenv currState node children = (newSizeReqW, newSizeReqH) where
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    newSizeReqW = getDimSizeReq isHorizontal (_wniSizeReqW . _wnInfo) vchildren
    newSizeReqH = getDimSizeReq isVertical (_wniSizeReqH . _wnInfo) vchildren

  getDimSizeReq mainAxis accesor vchildren
    | Seq.null vreqs = FixedSize 0
    | mainAxis = foldl1 sizeReqMergeSum vreqs
    | otherwise = foldl1 sizeReqMergeMax vreqs
    where
      vreqs = accesor <$> vchildren

  resize wenv viewport renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    (newViewports, newDim) = assignStackAreas isHorizontal contentArea children
    newCa
      | isHorizontal = contentArea & L.w .~ newDim
      | otherwise = contentArea & L.h .~ newDim
    newRa = fromMaybe newCa (addOuterBounds style newCa)
    newNode = node
      & L.info . L.viewport .~ newRa
      & L.info . L.renderArea .~ newRa
    assignedArea = Seq.zip newViewports newViewports
    resized = (newNode, assignedArea)

assignStackAreas :: Bool -> Rect -> Seq (WidgetNode s e) -> (Seq Rect, Double)
assignStackAreas isHorizontal contentArea children = result where
  Rect x y w h = contentArea
  mainSize = if isHorizontal then w else h
  mainStart = if isHorizontal then x else y
  rectSelector
    | isHorizontal = _rW
    | otherwise = _rH
  vchildren = Seq.filter (_wniVisible . _wnInfo) children
  reqs = fmap (mainReqSelector isHorizontal) vchildren
  sumSizes accum req = newStep where
    (cFixed, cFlex, cFlexFac, cExtraFac) = accum
    newFixed = cFixed + sizeReqFixed req
    newFlex = cFlex + sizeReqFlex req
    newFlexFac = cFlexFac + sizeReqFlex req * sizeReqFactor req
    newExtraFac = cExtraFac + sizeReqExtra req * sizeReqFactor req
    newStep = (newFixed, newFlex, newFlexFac, newExtraFac)
  (fixed, flex, flexFac, extraFac) = foldl' sumSizes def reqs
  flexAvail = min flex (mainSize - fixed)
  extraAvail = max 0 (mainSize - fixed - flexAvail)
  -- flexCoeff can only be negative
  flexCoeff
    | flexAvail < flex = (flexAvail - flex) / flexFac
    | otherwise = 0
  extraCoeff
    | extraAvail > 0 && extraFac > 0 = extraAvail / extraFac
    | otherwise = 0
  foldHelper (accum, offset) child = (newAccum, newOffset) where
    newSize = resizeChild isHorizontal contentArea flexCoeff extraCoeff offset child
    newAccum = accum |> newSize
    newOffset = offset + rectSelector newSize
  (areas, usedDim) = foldl' foldHelper (Seq.empty, mainStart) children
  result = (areas, usedDim - mainStart)

resizeChild :: Bool -> Rect -> Factor -> Factor -> Double -> WidgetNode s e -> Rect
resizeChild horizontal contentArea flexCoeff extraCoeff offset child = result where
  Rect l t w h = contentArea
  emptyRect = Rect l t 0 0
  -- Only one is active (flex is negative or extra is >= 0)
  totalCoeff = flexCoeff + extraCoeff
  tempMainSize = case mainReqSelector horizontal child of
    FixedSize sz -> sz
    FlexSize sz factor -> (1 + totalCoeff * factor) * sz
    -- Flex does not apply to min, so there's nothing to remove from (no '1 +')
    MinSize sz factor -> sz + extraCoeff * factor * sz
    MaxSize sz factor -> (1 + flexCoeff * factor) * sz
    RangeSize sz1 sz2 factor -> sz1 + (1 + flexCoeff * factor) * (sz2 - sz1)
  mainSize = max 0 tempMainSize
  hRect = Rect offset t mainSize h
  vRect = Rect l offset w mainSize
  result
    | not $ (_wniVisible . _wnInfo) child = emptyRect
    | horizontal = hRect
    | otherwise = vRect

mainReqSelector isHorizontal
  | isHorizontal = _wniSizeReqW . _wnInfo
  | otherwise = _wniSizeReqH . _wnInfo
