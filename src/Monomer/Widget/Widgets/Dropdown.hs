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

import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Color
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

newtype ItemEvent a = ItemClicked a

dropdown :: (Traversable t) => Lens' s a -> t a -> (a -> Text) -> WidgetInstance s e
dropdown field items display = makeInstance (makeDropdown Closed field overlayList) where
  overlayList = makeOverlayList items display

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = defaultWidgetInstance "dropdown" widget

makeOverlayList :: (Traversable t) => t a -> (a -> Text) -> WidgetInstance s (ItemEvent a)
makeOverlayList items display = scroll makeGrid where
  makeGrid = vstack $ fmap makeItem items
  makeItem i = container (config i) $ label (display i)
  config i = def {
    _onClick = Just $ ItemClicked i,
    _hoverColor = Just lightGray
  }

makeDropdown :: DropdownState -> Lens' s a -> WidgetInstance s (ItemEvent a) -> Widget s e
makeDropdown state field overlayInstance = createWidget {
    _widgetFind = dropdownFind,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResize = resize,
    _widgetRender = render
  }
  where
    dropdownLabel = "Hola"
    isOpen = state == Open
    createDropdown status = makeInstance $ makeDropdown status field overlayInstance

    dropdownFind point widgetComposite = fmap (0 <|) childPath where
      childPath = _widgetFind (_instanceWidget overlayInstance) point overlayInstance

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click p@(Point x y) _ status
        | clicked && openRequired p widgetInstance -> handleOpenDropdown ctx
        | clicked && closeRequired p widgetInstance -> handleCloseDropdown ctx
        where
          clicked = status == PressedBtn
      _
        | isOpen -> handleOverlayEvent wctx ctx evt widgetInstance
        | otherwise -> Nothing

    openRequired point widgetInstance = not isOpen && inViewport where
      inViewport = inRect (_instanceViewport widgetInstance) point

    closeRequired point widgetInstance = isOpen && not inOverlay where
      inOverlay = inRect (_instanceViewport overlayInstance) point

    handleOpenDropdown ctx = Just $ resultReqs requests newInstance where
      newInstance = createDropdown Open
      requests = [SetOverlay $ _pathCurrent ctx]

    handleCloseDropdown ctx = Just $ resultReqs requests newInstance where
      newInstance = createDropdown Closed
      requests = [ResetOverlay $ _pathCurrent ctx]

    handleOverlayEvent wctx ctx evt widgetInstance = result where
      cwctx = convertWidgetContext wctx
      cctx = childContext ctx
      childResult = _widgetHandleEvent (_instanceWidget overlayInstance) cwctx cctx evt overlayInstance
      result = case childResult of
        Just (WidgetResult reqs evts newOverlayList)
          | not (Seq.null evts) -> Just $ WidgetResult newReqs Seq.empty newInstance where
            newInstance = makeInstance $ makeDropdown state field newOverlayList
            newReqs = fmap convertItemClickReq evts
        _ -> Nothing

    convertItemClickReq (ItemClicked value) = UpdateUserState $ \app -> app & field .~ value

    preferredSize renderer wctx widgetInstance = Node sizeReq (Seq.singleton childReq) where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _textStyle dropdownLabel
      sizeReq = SizeReq size FlexibleSize StrictSize
      cwctx = convertWidgetContext wctx
      childReq = _widgetPreferredSize (_instanceWidget overlayInstance) renderer cwctx overlayInstance

    resize wctx viewport renderArea widgetInstance reqs = newInstance where
      newOverlayList = case Seq.lookup 0 (nodeChildren reqs) of
        Just reqChild -> resizedOverlay where
          reqHeight = _h . _sizeRequested . nodeValue $ reqChild
          maxHeight = min reqHeight 150
          oViewport = viewport { _ry = _ry viewport + _rh viewport, _rh = maxHeight }
          oRenderArea = renderArea { _ry = _ry renderArea + _rh viewport }
          cwctx = convertWidgetContext wctx
          resizedOverlay = _widgetResize (_instanceWidget overlayInstance) cwctx oViewport oRenderArea overlayInstance reqChild
        Nothing -> overlayInstance
      newInstance = widgetInstance {
        _instanceWidget = makeDropdown state field newOverlayList,
        _instanceViewport = viewport,
        _instanceRenderArea = renderArea
      }

    render renderer wctx ctx WidgetInstance{..} =
      do
        drawBgRect renderer _instanceRenderArea _instanceStyle
        drawText_ renderer _instanceRenderArea (_textStyle _instanceStyle) dropdownLabel

        when isOpen $
          createOverlay renderer $ renderOverlay renderer (convertWidgetContext wctx) ctx
    
    renderOverlay renderer wctx ctx = renderAction where
      renderAction = _widgetRender (_instanceWidget overlayInstance) renderer wctx ctx overlayInstance

convertWidgetContext :: WidgetContext s ep -> WidgetContext s e
convertWidgetContext wctx = WidgetContext {
  _wcScreenSize = _wcScreenSize wctx,
  _wcGlobalKeys = M.empty,
  _wcApp = _wcApp wctx,
  _wcInputStatus = _wcInputStatus wctx,
  _wcTimestamp = _wcTimestamp wctx
}

childContext :: PathContext -> PathContext
childContext ctx = addToCurrent ctx 0
