{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Dropdown (dropdown) where

import Debug.Trace

import Control.Monad
import Data.Default
import Data.List (foldl')
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Traversable
import Lens.Micro

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.BaseWidget
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.Container
import Monomer.Widget.Widgets.Label
import Monomer.Widget.Widgets.Scroll
import Monomer.Widget.Widgets.Spacer
import Monomer.Widget.Widgets.Stack

data DropdownState = Open | Closed deriving (Eq, Show)

data ItemEvent a = ItemClicked a

dropdown :: (Traversable t) => Lens' s a -> t a -> (a -> Text) -> WidgetInstance s e
dropdown field items display = makeInstance (makeDropdown Closed field overlayList) where
  overlayList = makeOverlayList items display

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = defaultWidgetInstance "dropdown" widget

makeOverlayList :: (Traversable t) => t a -> (a -> Text) -> WidgetInstance s (ItemEvent a)
makeOverlayList items display = scroll makeGrid where
  makeGrid = vstack $ fmap makeItem items
  makeItem i = container def { _onClick = Just $ ItemClicked i } $ label (display i)

makeDropdown :: DropdownState -> Lens' s a -> WidgetInstance s (ItemEvent a) -> Widget s  ae
makeDropdown state field overlayList = createWidget {
    _widgetFind = dropdownFind,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResize = resize,
    _widgetRender = render
  }
  where
    dropdownLabel = "Hola"
    isOpen = state == Open
    createDropdown status = makeInstance $ makeDropdown status field overlayList

    dropdownFind point widgetComposite = fmap (0 <|) childPath where
      childPath = _widgetFind (_instanceWidget overlayList) point overlayList

    handleEvent ctx evt app widgetInstance = case evt of
      Click p@(Point x y) _ status -> Just $ resultReqs requests newInstance where
        inViewport = inRect (_instanceViewport widgetInstance) p
        inOverlay = inRect (_instanceViewport overlayList) p
        isOpenAction = status == PressedBtn && not isOpen && inViewport
        isCloseAction = status == PressedBtn && isOpen && not inViewport 
        newInstance = if | isOpenAction  -> createDropdown Open
                         | isCloseAction -> createDropdown Closed
                         | otherwise -> widgetInstance
        requests = if | isOpenAction -> [SetOverlay $ _pathCurrent ctx]
                      | isCloseAction -> [ResetOverlay $ _pathCurrent ctx]
                      | otherwise -> []
      _ -> Nothing
      -- Nothing where
      -- !childRes = _widgetHandleEvent (_instanceWidget overlayList) (childContext ctx) evt app overlayList

    preferredSize renderer app widgetInstance = Node sizeReq (Seq.singleton childReq) where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _textStyle dropdownLabel
      sizeReq = SizeReq size FlexibleSize StrictSize
      childReq = _widgetPreferredSize (_instanceWidget overlayList) renderer app overlayList

    resize app viewport renderArea widgetInstance reqs = newInstance where
      newOverlayList = case Seq.lookup 0 (nodeChildren reqs) of
        Just reqChild -> resizedOverlay where
          reqHeight = _h . _sizeRequested . nodeValue $ reqChild
          maxHeight = min reqHeight 150
          oViewport = viewport { _ry = _ry viewport + _rh viewport, _rh = maxHeight }
          oRenderArea = renderArea { _ry = _ry renderArea + _rh viewport }
          resizedOverlay = _widgetResize (_instanceWidget overlayList) app oViewport oRenderArea overlayList reqChild
        Nothing -> overlayList
      newInstance = widgetInstance {
        _instanceWidget = makeDropdown state field newOverlayList,
        _instanceViewport = viewport,
        _instanceRenderArea = renderArea
      }

    render renderer ts ctx app WidgetInstance{..} =
      do
        drawBgRect renderer _instanceRenderArea _instanceStyle
        drawText_ renderer _instanceRenderArea (_textStyle _instanceStyle) dropdownLabel

        when isOpen $
          createOverlay renderer $ renderOverlay renderer ts ctx app
    
    renderOverlay renderer ts ctx app = renderAction where
      renderAction = _widgetRender (_instanceWidget overlayList) renderer ts ctx app overlayList
