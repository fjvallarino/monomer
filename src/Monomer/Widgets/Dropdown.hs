{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Dropdown (
  DropdownCfg,
  DropdownItem(..),
  dropdown,
  dropdown_,
  dropdownV,
  dropdownV_,
  dropdownD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^#), (#~), (^.), (^?), (.~), _Just, non)
import Control.Monad
import Data.Default
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Label
import Monomer.Widgets.ListView

import qualified Monomer.Lens as L

type DropdownItem a = ListItem a

data DropdownCfg s e a = DropdownCfg {
  _ddcMaxHeight :: Maybe Double,
  _ddcListStyle :: Maybe Style,
  _ddcItemStyle :: Maybe Style,
  _ddcItemSelectedStyle :: Maybe Style,
  _ddcOnChange :: [a -> e],
  _ddcOnChangeReq :: [WidgetRequest s],
  _ddcOnChangeIdx :: [Int -> a -> e],
  _ddcOnChangeIdxReq :: [Int -> WidgetRequest s]
}

instance Default (DropdownCfg s e a) where
  def = DropdownCfg {
    _ddcMaxHeight = Nothing,
    _ddcListStyle = Nothing,
    _ddcItemStyle = Nothing,
    _ddcItemSelectedStyle = Nothing,
    _ddcOnChange = [],
    _ddcOnChangeReq = [],
    _ddcOnChangeIdx = [],
    _ddcOnChangeIdxReq = []
  }

instance Semigroup (DropdownCfg s e a) where
  (<>) t1 t2 = DropdownCfg {
    _ddcMaxHeight = _ddcMaxHeight t2 <|> _ddcMaxHeight t1,
    _ddcListStyle = _ddcListStyle t2 <|> _ddcListStyle t1,
    _ddcItemStyle = _ddcItemStyle t2 <|> _ddcItemStyle t1,
    _ddcItemSelectedStyle = _ddcItemSelectedStyle t2 <|> _ddcItemSelectedStyle t1,
    _ddcOnChange = _ddcOnChange t1 <> _ddcOnChange t2,
    _ddcOnChangeReq = _ddcOnChangeReq t1 <> _ddcOnChangeReq t2,
    _ddcOnChangeIdx = _ddcOnChangeIdx t1 <> _ddcOnChangeIdx t2,
    _ddcOnChangeIdxReq = _ddcOnChangeIdxReq t1 <> _ddcOnChangeIdxReq t2
  }

instance Monoid (DropdownCfg s e a) where
  mempty = def

instance CmbOnChange (DropdownCfg s e a) a e where
  onChange fn = def {
    _ddcOnChange = [fn]
  }

instance CmbOnChangeReq (DropdownCfg s e a) s where
  onChangeReq req = def {
    _ddcOnChangeReq = [req]
  }

instance CmbOnChangeIdx (DropdownCfg s e a) a e where
  onChangeIdx fn = def {
    _ddcOnChangeIdx = [fn]
  }

instance CmbOnChangeIdxReq (DropdownCfg s e a) s where
  onChangeIdxReq req = def {
    _ddcOnChangeIdxReq = [req]
  }

instance CmbMaxHeight (DropdownCfg s e a) where
  maxHeight h = def {
    _ddcMaxHeight = Just h
  }

instance CmbItemListStyle (DropdownCfg s e a) Style where
  itemListStyle style = def {
    _ddcListStyle = Just style
  }

instance CmbItemNormalStyle (DropdownCfg s e a) Style where
  itemNormalStyle style = def {
    _ddcItemStyle = Just style
  }

instance CmbItemSelectedStyle (DropdownCfg s e a) Style where
  itemSelectedStyle style = def {
    _ddcItemSelectedStyle = Just style
  }

newtype DropdownState = DropdownState {
  _isOpen :: Bool
}

data DropdownMessage
  = OnChangeMessage Int
  | OnListBlur
  deriving Typeable

dropdown
  :: (Traversable t, DropdownItem a)
  => ALens' s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> (a -> WidgetInstance s e)
  -> WidgetInstance s e
dropdown field items makeMain makeRow = newInst where
  newInst = dropdown_ field items makeMain makeRow def

dropdown_
  :: (Traversable t, DropdownItem a)
  => ALens' s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> (a -> WidgetInstance s e)
  -> [DropdownCfg s e a]
  -> WidgetInstance s e
dropdown_ field items makeMain makeRow configs = newInst where
  widgetData = WidgetLens field
  newInst = dropdownD_ widgetData items makeMain makeRow configs

dropdownV
  :: (Traversable t, DropdownItem a)
  => a
  -> (a -> e)
  -> t a
  -> (a -> WidgetInstance s e)
  -> (a -> WidgetInstance s e)
  -> WidgetInstance s e
dropdownV value handler items makeMain makeRow = newInst where
  newInst = dropdownV_ value handler items makeMain makeRow def

dropdownV_
  :: (Traversable t, DropdownItem a)
  => a
  -> (a -> e)
  -> t a
  -> (a -> WidgetInstance s e)
  -> (a -> WidgetInstance s e)
  -> [DropdownCfg s e a]
  -> WidgetInstance s e
dropdownV_ value handler items makeMain makeRow configs = newInst where
  newConfigs = onChange handler : configs
  newInst = dropdownD_ (WidgetValue value) items makeMain makeRow newConfigs

dropdownD_
  :: (Traversable t, DropdownItem a)
  => WidgetData s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> (a -> WidgetInstance s e)
  -> [DropdownCfg s e a]
  -> WidgetInstance s e
dropdownD_ widgetData items makeMain makeRow configs = makeInstance widget where
  config = mconcat configs
  newState = DropdownState False
  newItems = foldl' (|>) Empty items
  widget = makeDropdown widgetData newItems makeMain makeRow config newState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "dropdown" widget) {
  _wiFocusable = True
}

makeDropdown
  :: DropdownItem a
  => WidgetData s a
  -> Seq a
  -> (a -> WidgetInstance s e)
  -> (a -> WidgetInstance s e)
  -> DropdownCfg s e a
  -> DropdownState
  -> Widget s e
makeDropdown widgetData items makeMain makeRow config state = widget where
  baseWidget = createContainer def {
    containerGetBaseStyle = getBaseStyle,
    containerInit = init,
    containerGetState = makeState state,
    containerFindNextFocus = findNextFocus,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  mainIdx = 0
  listIdx = 1
  isOpen = _isOpen state
  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createDropdown wenv newState inst = newInst where
    selected = currentValue wenv
    mainStyle = collectTheme wenv L.dropdownStyle
    mainInst = makeMain selected & L.style .~ mainStyle
    path = _wiPath inst
    listViewInst = makeListView wenv widgetData items makeRow config path
    newWidget = makeDropdown widgetData items makeMain makeRow config newState
    newInst = inst {
      _wiWidget = newWidget,
      _wiChildren = Seq.fromList [mainInst, listViewInst]
    }

  getBaseStyle wenv inst = Just style where
    style = collectTheme wenv L.dropdownStyle

  init wenv inst = resultWidget $ createDropdown wenv state inst

  merge wenv oldState oldInst newInst = result where
    newState = fromMaybe state (useState oldState)
    result = resultWidget $ createDropdown wenv newState newInst

  findNextFocus wenv direction start inst
    | _isOpen state = _wiChildren inst
    | otherwise = Empty

  handleEvent wenv target evt inst = case evt of
    Enter{} -> Nothing -- to have handleStyleChange applied
    Click point _
      | openRequired point inst -> Just $ openDropdown wenv inst
      | closeRequired point inst -> Just $ closeDropdown wenv inst
    KeyAction mode code status
      | isKeyOpenDropdown && not isOpen -> Just $ openDropdown wenv inst
      | isKeyEscape code && isOpen -> Just $ closeDropdown wenv inst
      where isKeyOpenDropdown = isKeyDown code || isKeyUp code
    _
      | not isOpen -> Just $ resultReqs inst [IgnoreChildrenEvents]
      | otherwise -> Nothing

  openRequired point inst = not isOpen && inViewport where
    inViewport = pointInRect point (_wiViewport inst)

  closeRequired point inst = isOpen && not inOverlay where
    inOverlay = case Seq.lookup listIdx (_wiChildren inst) of
      Just inst -> pointInRect point (_wiViewport inst)
      Nothing -> False

  openDropdown wenv inst = resultReqs newInst requests where
    selected = currentValue wenv
    selectedIdx = fromMaybe 0 (Seq.elemIndexL selected items)
    newState = DropdownState True
    newInst = inst {
      _wiWidget = makeDropdown widgetData items makeMain makeRow config newState
    }
    path = _wiPath inst
    -- listView is wrapped by a scroll widget
    lvPath = path |> listIdx |> 0
    requests = [SetOverlay path, SetFocus lvPath]

  closeDropdown wenv inst = resultReqs newInst requests where
    path = _wiPath inst
    newState = DropdownState False
    newInst = inst {
      _wiWidget = makeDropdown widgetData items makeMain makeRow config newState
    }
    requests = [ResetOverlay, SetFocus path]

  handleMessage wenv target msg inst =
    cast msg >>= handleLvMsg wenv inst

  handleLvMsg wenv inst (OnChangeMessage idx) =
    Seq.lookup idx items >>= \value -> Just $ onChange wenv idx value inst
  handleLvMsg wenv inst OnListBlur = Just result where
    tempResult = closeDropdown wenv inst
    result = tempResult {
      _wrRequests = _wrRequests tempResult |> createMoveFocusReq wenv
    }

  onChange wenv idx item inst = result where
    WidgetResult newInst reqs events = closeDropdown wenv inst
    newReqs = Seq.fromList $ widgetDataSet widgetData item
      ++ _ddcOnChangeReq config
      ++ fmap ($ idx) (_ddcOnChangeIdxReq config)
    newEvents = Seq.fromList $ fmap ($ item) (_ddcOnChange config)
      ++ fmap (\fn -> fn idx item) (_ddcOnChangeIdx config)
    result = WidgetResult newInst (reqs <> newReqs) (events <> newEvents)

  getSizeReq wenv inst children = sizeReq where
    child = Seq.index children 0
    newChild = widgetUpdateSizeReq (_wiWidget child) wenv child
    sizeReq = (_wiSizeReqW newChild, _wiSizeReqH newChild)

  resize wenv viewport renderArea children inst = resized where
    Size winW winH = _weAppWindowSize wenv
    Rect rx ry rw rh = renderArea
    theme = activeTheme wenv inst
    dropdownY dh
      | ry + rh + dh <= winH = ry + rh
      | otherwise = ry - dh
    listArea = case Seq.lookup 1 children of
      Just child -> (oViewport, oRenderArea) where
        maxHeightTheme = theme ^. L.dropdownMaxHeight
        maxHeightStyle = fromMaybe maxHeightTheme (_ddcMaxHeight config)
        reqHeight = sizeReqMin . _wiSizeReqH $ child
        maxHeight = min reqHeight maxHeightStyle
        oViewport = viewport {
          _rY = dropdownY maxHeight,
          _rH = maxHeight
        }
        oRenderArea = renderArea {
          _rY = dropdownY maxHeight,
          _rH = maxHeight
        }
      Nothing -> (viewport, renderArea)
    mainArea = (viewport, renderArea)
    assignedAreas = Seq.fromList [mainArea, listArea]
    resized = (inst, assignedAreas)

  render renderer wenv inst = do
    drawInScissor renderer True viewport $
      drawStyledAction renderer renderArea style $ \contentArea -> do
        widgetRender (_wiWidget mainInst) renderer wenv mainInst
        renderArrow renderer style contentArea

    when (isOpen && isJust listViewOverlay) $
      createOverlay renderer $
        renderOverlay renderer wenv (fromJust listViewOverlay)
    where
      style = activeStyle wenv inst
      viewport = _wiViewport inst
      renderArea = _wiRenderArea inst
      mainInst = Seq.index (_wiChildren inst) mainIdx
      listViewOverlay = Seq.lookup listIdx (_wiChildren inst)

  renderArrow renderer style contentArea =
    drawArrowDown renderer arrowRect (_sstFgColor style)
    where
      Rect x y w h = contentArea
      size = style ^. L.text . non def . L.fontSize . non def
      arrowW = unFontSize size / 2
      dh = (h - arrowW) / 2
      arrowRect = Rect (x + w - dh) (y + dh * 1.25) arrowW (arrowW / 2)

  renderOverlay renderer wenv overlayInstance = renderAction where
    widget = _wiWidget overlayInstance
    renderAction = widgetRender widget renderer wenv overlayInstance

makeListView
  :: DropdownItem a
  => WidgetEnv s e
  -> WidgetData s a
  -> Seq a
  -> (a -> WidgetInstance s e)
  -> DropdownCfg s e a
  -> Path
  -> WidgetInstance s e
makeListView wenv value items makeRow config path = listViewInst where
  normalTheme = collectTheme wenv L.dropdownItemStyle
  selectedTheme = collectTheme wenv L.dropdownItemSelectedStyle
  itemStyle = fromJust (Just normalTheme <> _ddcItemStyle config)
  itemSelStyle = fromJust (Just selectedTheme <> _ddcItemSelectedStyle config)
  lvConfig = [
      selectOnBlur True,
      onChangeIdxReq (SendMessage path . OnChangeMessage),
      itemNormalStyle itemStyle,
      itemSelectedStyle itemSelStyle
    ]
  lvStyle = collectTheme wenv L.dropdownListStyle
  listViewInst = listViewD_ value items makeRow lvConfig & L.style .~ lvStyle

createMoveFocusReq :: WidgetEnv s e -> WidgetRequest s
createMoveFocusReq wenv = MoveFocus direction where
  direction
    | wenv ^. L.inputStatus . L.keyMod . L.leftShift = FocusBwd
    | otherwise = FocusFwd
