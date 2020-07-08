{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Dropdown (dropdown) where

import Debug.Trace

import Control.Applicative ((<|>))
import Control.Lens (Lens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Foldable (find)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Traversable

import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Keyboard
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

data Dropdown = Dropdown {
  _isOpen :: Bool,
  _highlighted :: Int
}

newtype ItemEvent a = ItemClicked a

dropdown :: (Traversable t, Eq a) => Lens' s a -> t a -> (a -> Text) -> WidgetInstance s e
dropdown field items itemToText = makeInstance (makeDropdown newStatus field newItems itemToText spacer) where
  newItems = foldl' (|>) Empty items
  newStatus = Dropdown False 0

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "dropdown" widget) {
  _instanceFocusable = True
}

makeDropdown :: (Eq a) => Dropdown -> Lens' s a -> Seq a -> (a -> Text) -> WidgetInstance s (ItemEvent a) -> Widget s e
makeDropdown state field items itemToText overlayInstance = createWidget {
    _widgetInit = init,
    _widgetMerge = merge,
    _widgetFind = find,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetResize = resize,
    _widgetRender = render
  }
  where
    isOpen = _isOpen state
    createDropdown wctx ctx newState = newInstance where
      selected = _wcApp wctx ^. field
      newOverlayList = makeOverlayList items selected (_highlighted newState) itemToText
      newInstance = makeInstance $ makeDropdown newState field items itemToText newOverlayList

    init wctx ctx widgetInstance = resultWidget $ createDropdown wctx ctx state
    merge wctx ctx oldInstance newInstance = resultWidget $ createDropdown wctx ctx state

    find path point widgetInstance
      | validStep = fmap (0 <|) childPath
      | otherwise = Nothing
      where
        validStep = Seq.null path || Seq.index path 0 == 0
        newPath = Seq.drop 1 path
        childPath = _widgetFind (_instanceWidget overlayInstance) newPath point overlayInstance

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click p@(Point x y) _ status
        | clicked && openRequired p widgetInstance -> handleOpenDropdown wctx ctx
        | clicked && closeRequired p widgetInstance -> handleCloseDropdown wctx ctx
        where
          clicked = status == PressedBtn
      KeyAction mode code status
        | isKeyDown code && not isOpen -> handleOpenDropdown wctx ctx
        | isKeyDown code && status == KeyPressed && isOpen -> handleSelectNext wctx ctx
        | isKeyUp code && status == KeyPressed && isOpen -> handleSelectPrev wctx ctx
        | isKeyReturn code && status == KeyPressed && isOpen -> handleSelectHighligted wctx ctx
        | isKeyEsc code && isOpen -> handleCloseDropdown wctx ctx
      _
        | isOpen -> handleOverlayEvent wctx ctx evt widgetInstance
        | otherwise -> Nothing

    openRequired point widgetInstance = not isOpen && inViewport where
      inViewport = inRect (_instanceViewport widgetInstance) point

    closeRequired point widgetInstance = isOpen && not inOverlay where
      inOverlay = inRect (_instanceViewport overlayInstance) point

    handleOpenDropdown wctx ctx = Just $ resultReqs requests newInstance where
      selected = _wcApp wctx ^. field
      selectedIdx = fromMaybe 0 (Seq.elemIndexL selected items)
      newInstance = createDropdown wctx ctx $ Dropdown True selectedIdx
      requests = [SetOverlay $ _pathCurrent ctx]

    handleCloseDropdown wctx ctx = Just $ resultReqs requests newInstance where
      newInstance = createDropdown wctx ctx $ Dropdown False 0
      requests = [ResetOverlay]

    handleSelectNext wctx ctx = Just $ resultWidget newInstance where
      tempIdx = _highlighted state
      nextIdx = if tempIdx < length items - 1 then tempIdx + 1 else tempIdx
      newInstance = createDropdown wctx ctx $ Dropdown True nextIdx

    handleSelectPrev wctx ctx = Just $ resultWidget newInstance where
      tempIdx = _highlighted state
      nextIdx = if tempIdx > 0 then tempIdx - 1 else tempIdx
      newInstance = createDropdown wctx ctx $ Dropdown True nextIdx

    handleSelectHighligted wctx ctx = Just $ resultReqs requests newInstance where
      selected = _wcApp wctx ^. field
      idx = _highlighted state
      value = fromMaybe selected (Seq.lookup idx items)
      requests = [UpdateUserState $ \app -> app & field .~ value]
      newInstance = createDropdown wctx ctx $ Dropdown False 0

    handleOverlayEvent wctx ctx evt widgetInstance = result where
      resetReq = ResetOverlay
      cwctx = convertWidgetContext wctx
      cctx = childContext ctx
      childResult = _widgetHandleEvent (_instanceWidget overlayInstance) cwctx cctx evt overlayInstance
      result = case childResult of
        Just (WidgetResult reqs evts newOverlayList)
          | not (Seq.null evts) -> Just $ WidgetResult newReqs Seq.empty newInstance
          | otherwise -> Just $ resultWidget newInstance
          where
            newReqs = resetReq <| fmap convertItemClickReq evts
            newInstance = makeInstance $ makeDropdown state field items itemToText newOverlayList
        _ -> Nothing

    convertItemClickReq (ItemClicked value) = UpdateUserState $ \app -> app & field .~ value

    dropdownLabel wctx = if value == "" then " " else value where
      value = itemToText $ _wcApp wctx ^. field

    preferredSize renderer wctx widgetInstance = Node sizeReq (Seq.singleton childReq) where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _styleText (dropdownLabel wctx)
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
        _instanceWidget = makeDropdown state field items itemToText newOverlayList,
        _instanceViewport = viewport,
        _instanceRenderArea = renderArea
      }

    render renderer wctx ctx WidgetInstance{..} =
      do
        drawStyledBackground renderer _instanceRenderArea _instanceStyle
        drawStyledText_ renderer _instanceRenderArea _instanceStyle (dropdownLabel wctx)

        when isOpen $
          createOverlay renderer $ renderOverlay renderer (convertWidgetContext wctx) ctx
    
    renderOverlay renderer wctx ctx = renderAction where
      renderAction = _widgetRender (_instanceWidget overlayInstance) renderer wctx ctx overlayInstance

makeOverlayList :: (Eq a) => Seq a -> a -> Int -> (a -> Text) -> WidgetInstance s (ItemEvent a)
makeOverlayList items selected highlightedIdx itemToText = scroll makeGrid where
  isSelected item = item == selected
  selectedColor item = if isSelected item then Just gray else Nothing
  highlightedColor idx = if idx == highlightedIdx then Just darkGray else Nothing
  pairs = Seq.zip (Seq.fromList [0..length items]) items
  makeGrid = vstack $ fmap (uncurry makeItem) pairs
  makeItem idx item = container (config idx item) $ label (itemToText item)
  config idx item = def {
    _ctOnClick = [ItemClicked item],
    _ctBgColor = highlightedColor idx <|> selectedColor item,
    _ctHoverColor = Just lightGray
  }

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
