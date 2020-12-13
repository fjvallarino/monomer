module Monomer.Widgets.Stack (
  hstack,
  hstack_,
  vstack,
  vstack_
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

instance CmbIgnoreEmptyClick StackCfg where
  ignoreEmptyClick ignore = def {
    _stcIgnoreEmptyClick = Just ignore
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
  widget = createContainer def {
    containerIgnoreEmptyClick = ignoreEmptyClick,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  ignoreEmptyClick = _stcIgnoreEmptyClick config == Just True
  isVertical = not isHorizontal

  getSizeReq wenv node children = (newSizeReqW, newSizeReqH) where
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
    Rect x y w h = contentArea
    mainSize = if isHorizontal then w else h
    mainStart = if isHorizontal then x else y
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    reqs = fmap mainReqSelector vchildren
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
      newSize = resizeChild contentArea flexCoeff extraCoeff offset child
      newAccum = accum |> newSize
      newOffset = offset + rectSelector newSize
    (newViewports, _) = foldl' foldHelper (Seq.empty, mainStart) children
    assignedArea = Seq.zip newViewports newViewports
    resized = (node, assignedArea)

  resizeChild contentArea flexCoeff extraCoeff offset child = result where
    Rect l t w h = contentArea
    emptyRect = Rect l t 0 0
    -- Only one is active (flex is negative or extra is >= 0)
    totalCoeff = flexCoeff + extraCoeff
    tempMainSize = case mainReqSelector child of
      FixedSize sz -> sz
      FlexSize sz factor -> (1 + totalCoeff * factor) * sz
      -- Since flex does not apply to min, there's nothing to remove from (no '1 +')
      MinSize sz factor -> sz + extraCoeff * factor * sz
      MaxSize sz factor -> (1 + flexCoeff * factor) * sz
      RangeSize sz1 sz2 factor -> sz1 + (1 + flexCoeff * factor) * (sz2 - sz1)
    mainSize = max 0 tempMainSize
    hRect = Rect offset t mainSize h
    vRect = Rect l offset w mainSize
    result
      | not $ (_wniVisible . _wnInfo) child = emptyRect
      | isHorizontal = hRect
      | otherwise = vRect

  mainReqSelector
    | isHorizontal = _wniSizeReqW . _wnInfo
    | otherwise = _wniSizeReqH . _wnInfo

  rectSelector
    | isHorizontal = _rW
    | otherwise = _rH
