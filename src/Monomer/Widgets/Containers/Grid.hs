{-|
Module      : Monomer.Widgets.Containers.Grid
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Layout container which distributes size equally along the main axis. For hgrid
it requests max width * elements as its width, and the max height as its height.
The reverse happens for vgrid.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.Grid (
  -- * Configuration
  GridCfg,
  -- * Constructors
  hgrid,
  hgrid_,
  vgrid,
  vgrid_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~))
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Helper (applyFnList)
import Monomer.Widgets.Container

import qualified Monomer.Lens as L

{-|
Configuration options for grid:

- 'childSpacing': spacing between the child widgets.
- 'sizeReqUpdater': allows modifying the 'SizeReq' generated by the grid.
-}
data GridCfg = GridCfg {
  _grcChildSpacing :: Maybe Double,
  _grcSizeReqUpdater :: [SizeReqUpdater]
}

instance Default GridCfg where
  def = GridCfg {
    _grcChildSpacing = Nothing,
    _grcSizeReqUpdater = []
  }

instance Semigroup GridCfg where
  (<>) s1 s2 = GridCfg {
    _grcChildSpacing = _grcChildSpacing s2 <|> _grcChildSpacing s1,
    _grcSizeReqUpdater = _grcSizeReqUpdater s1 <> _grcSizeReqUpdater s2
  }

instance Monoid GridCfg where
  mempty = def

instance CmbChildSpacing GridCfg where
  childSpacing_ spacing = def {
    _grcChildSpacing = Just spacing
  }

instance CmbSizeReqUpdater GridCfg where
  sizeReqUpdater updater = def {
    _grcSizeReqUpdater = [updater]
  }

-- | Creates a grid of items with the same width.
hgrid :: Traversable t => t (WidgetNode s e) -> WidgetNode s e
hgrid children = hgrid_ def children

-- | Creates a grid of items with the same width. Accepts config.
hgrid_ :: Traversable t => [GridCfg] -> t (WidgetNode s e) -> WidgetNode s e
hgrid_ configs children = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "hgrid" (makeFixedGrid True config)
    & L.children .~ foldl' (|>) Empty children

-- | Creates a grid of items with the same height.
vgrid :: Traversable t => t (WidgetNode s e) -> WidgetNode s e
vgrid children = vgrid_ def children

-- | Creates a grid of items with the same height. Accepts config.
vgrid_ :: Traversable t => [GridCfg] -> t (WidgetNode s e) -> WidgetNode s e
vgrid_ configs children = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "vgrid" (makeFixedGrid False config)
    & L.children .~ foldl' (|>) Empty children

makeFixedGrid :: Bool -> GridCfg -> Widget s e
makeFixedGrid isHorizontal config = widget where
  widget = createContainer () def {
    containerLayoutDirection = getLayoutDirection isHorizontal,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  isVertical = not isHorizontal
  childSpacing = fromMaybe 0 (_grcChildSpacing config)

  getSizeReq wenv node children = newSizeReq where
    sizeReqFns = _grcSizeReqUpdater config
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    newSizeReqW = getDimSizeReq isHorizontal (_wniSizeReqW . _wnInfo) vchildren
    newSizeReqH = getDimSizeReq isVertical (_wniSizeReqH . _wnInfo) vchildren
    newSizeReq = applyFnList sizeReqFns (newSizeReqW, newSizeReqH)

  getDimSizeReq mainAxis accesor vchildren
    | Seq.null vreqs = fixedSize 0
    | mainAxis = foldl1 sizeReqMergeSum (Seq.replicate nreqs maxSize) & L.fixed %~ (+ totalSpacing)
    | otherwise = maxSize
    where
      vreqs = accesor <$> vchildren
      nreqs = Seq.length vreqs
      ~maxSize = foldl1 sizeReqMergeMax vreqs
      totalSpacing = fromIntegral (nreqs - 1) * childSpacing

  resize wenv node viewport children = resized where
    style = currentStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style viewport)
    Rect l t w h = contentArea
    vchildren = Seq.filter (_wniVisible . _wnInfo) children

    totalSpacingW = fromIntegral (max 0 (cols - 1)) * childSpacing
    totalSpacingH = fromIntegral (max 0 (rows - 1)) * childSpacing

    cols = if isHorizontal then length vchildren else 1
    rows = if isHorizontal then 1 else length vchildren

    cw = if cols > 0 then (w - totalSpacingW) / fromIntegral cols else 0
    ch = if rows > 0 then (h - totalSpacingH) / fromIntegral rows else 0

    cx i
      | rows == 0 = 0
      | isHorizontal = l + fromIntegral (i `div` rows) * cw + spacingOffset i
      | otherwise = l
    cy i
      | cols == 0 = 0
      | isVertical = t + fromIntegral (i `div` cols) * ch + spacingOffset i
      | otherwise = t
    spacingOffset i =
      fromIntegral i * childSpacing

    foldHelper (currAreas, index) child = (newAreas, newIndex) where
      (newIndex, newViewport)
        | child ^. L.info . L.visible = (index + 1, calcViewport index)
        | otherwise = (index, def)
      newArea = newViewport
      newAreas = currAreas |> newArea
    calcViewport i = Rect (cx i) (cy i) cw ch

    assignedAreas = fst $ foldl' foldHelper (Seq.empty, 0) children
    resized = (resultNode node, assignedAreas)
