module Monomer.Widgets.Stack (
  hstack,
  vstack
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

hstack :: (Traversable t) => t (WidgetNode s e) -> WidgetNode s e
hstack children = newNode where
  newNode = defaultWidgetNode "hstack" (makeStack True)
    & L.children .~ foldl' (|>) Empty children

vstack :: (Traversable t) => t (WidgetNode s e) -> WidgetNode s e
vstack children = newNode where
  newNode = defaultWidgetNode "vstack" (makeStack False)
    & L.children .~ foldl' (|>) Empty children

makeStack :: Bool -> Widget s e
makeStack isHorizontal = widget where
  widget = createContainer def {
    containerUseCustomSize = True,
    containerFindByPoint = defaultFindByPoint,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

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
    (newViewports, newDim) = foldl' foldHelper (Seq.empty, mainStart) children
    newCa
      | isHorizontal = contentArea & L.w .~ newDim
      | otherwise = contentArea & L.h .~ newDim
    newRa = fromMaybe newCa (addOuterBounds style newCa)
    newNode = node
      & L.info . L.viewport .~ newRa
      & L.info . L.renderArea .~ newRa
    assignedArea = Seq.zip newViewports newViewports
    resized = (newNode, assignedArea)

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
