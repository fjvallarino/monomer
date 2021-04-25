{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Singles.Dropdown (
  DropdownCfg,
  DropdownItem(..),
  dropdown,
  dropdown_,
  dropdownV,
  dropdownV_,
  dropdownD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (^?), (^?!), (.~), (%~), (<>~), _Just, ix, non)
import Control.Monad
import Data.Default
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (cast)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.ListView

import qualified Monomer.Lens as L

type DropdownItem a = ListItem a

data DropdownCfg s e a = DropdownCfg {
  _ddcMaxHeight :: Maybe Double,
  _ddcListStyle :: Maybe Style,
  _ddcItemStyle :: Maybe Style,
  _ddcItemSelectedStyle :: Maybe Style,
  _ddcOnFocus :: [e],
  _ddcOnFocusReq :: [WidgetRequest s e],
  _ddcOnBlur :: [e],
  _ddcOnBlurReq :: [WidgetRequest s e],
  _ddcOnChange :: [a -> e],
  _ddcOnChangeReq :: [WidgetRequest s e],
  _ddcOnChangeIdx :: [Int -> a -> e],
  _ddcOnChangeIdxReq :: [Int -> WidgetRequest s e]
}

instance Default (DropdownCfg s e a) where
  def = DropdownCfg {
    _ddcMaxHeight = Nothing,
    _ddcListStyle = Nothing,
    _ddcItemStyle = Nothing,
    _ddcItemSelectedStyle = Nothing,
    _ddcOnFocus = [],
    _ddcOnFocusReq = [],
    _ddcOnBlur = [],
    _ddcOnBlurReq = [],
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
    _ddcOnFocus = _ddcOnFocus t1 <> _ddcOnFocus t2,
    _ddcOnFocusReq = _ddcOnFocusReq t1 <> _ddcOnFocusReq t2,
    _ddcOnBlur = _ddcOnBlur t1 <> _ddcOnBlur t2,
    _ddcOnBlurReq = _ddcOnBlurReq t1 <> _ddcOnBlurReq t2,
    _ddcOnChange = _ddcOnChange t1 <> _ddcOnChange t2,
    _ddcOnChangeReq = _ddcOnChangeReq t1 <> _ddcOnChangeReq t2,
    _ddcOnChangeIdx = _ddcOnChangeIdx t1 <> _ddcOnChangeIdx t2,
    _ddcOnChangeIdxReq = _ddcOnChangeIdxReq t1 <> _ddcOnChangeIdxReq t2
  }

instance Monoid (DropdownCfg s e a) where
  mempty = def

instance CmbOnFocus (DropdownCfg s e a) e where
  onFocus fn = def {
    _ddcOnFocus = [fn]
  }

instance CmbOnFocusReq (DropdownCfg s e a) s e where
  onFocusReq req = def {
    _ddcOnFocusReq = [req]
  }

instance CmbOnBlur (DropdownCfg s e a) e where
  onBlur fn = def {
    _ddcOnBlur = [fn]
  }

instance CmbOnBlurReq (DropdownCfg s e a) s e where
  onBlurReq req = def {
    _ddcOnBlurReq = [req]
  }

instance CmbOnChange (DropdownCfg s e a) a e where
  onChange fn = def {
    _ddcOnChange = [fn]
  }

instance CmbOnChangeReq (DropdownCfg s e a) s e where
  onChangeReq req = def {
    _ddcOnChangeReq = [req]
  }

instance CmbOnChangeIdx (DropdownCfg s e a) a e where
  onChangeIdx fn = def {
    _ddcOnChangeIdx = [fn]
  }

instance CmbOnChangeIdxReq (DropdownCfg s e a) s e where
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

data DropdownState = DropdownState {
  _ddsOpen :: Bool,
  _ddsOffset :: Point
} deriving (Eq, Show, Generic)

data DropdownMessage
  = OnChangeMessage Int
  | OnListBlur

dropdown
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => ALens' s a
  -> t a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> WidgetNode s e
dropdown field items makeMain makeRow = newNode where
  newNode = dropdown_ field items makeMain makeRow def

dropdown_
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => ALens' s a
  -> t a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
dropdown_ field items makeMain makeRow configs = newNode where
  widgetData = WidgetLens field
  newNode = dropdownD_ widgetData items makeMain makeRow configs

dropdownV
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => a
  -> (Int -> a -> e)
  -> t a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> WidgetNode s e
dropdownV value handler items makeMain makeRow = newNode where
  newNode = dropdownV_ value handler items makeMain makeRow def

dropdownV_
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => a
  -> (Int -> a -> e)
  -> t a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
dropdownV_ value handler items makeMain makeRow configs = newNode where
  newConfigs = onChangeIdx handler : configs
  newNode = dropdownD_ (WidgetValue value) items makeMain makeRow newConfigs

dropdownD_
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => WidgetData s a
  -> t a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
dropdownD_ widgetData items makeMain makeRow configs = makeNode widget where
  config = mconcat configs
  newState = DropdownState False def
  newItems = foldl' (|>) Empty items
  widget = makeDropdown widgetData newItems makeMain makeRow config newState

makeNode :: Widget s e -> WidgetNode s e
makeNode widget = defaultWidgetNode "dropdown" widget
  & L.info . L.focusable .~ True

makeDropdown
  :: (DropdownItem a, WidgetEvent e)
  => WidgetData s a
  -> Seq a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> DropdownCfg s e a
  -> DropdownState
  -> Widget s e
makeDropdown widgetData items makeMain makeRow config state = widget where
  container = def {
    containerChildrenOffset = Just (_ddsOffset state),
    containerUseCustomCursor = True,
    containerGetBaseStyle = getBaseStyle,
    containerInit = init,
    containerFindNextFocus = findNextFocus,
    containerFindByPoint = findByPoint,
    containerMerge = merge,
    containerDispose = dispose,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  baseWidget = createContainer state container
  widget = baseWidget {
    widgetRender = render
  }

  mainIdx = 0
  listIdx = 1
  isOpen = _ddsOpen state
  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createDropdown wenv node newState = newNode where
    selected = currentValue wenv
    mainStyle = collectTheme wenv L.dropdownStyle
    mainNode = makeMain selected
      & L.info . L.style .~ mainStyle
    widgetId = node ^. L.info . L.widgetId
    listViewNode = makeListView wenv widgetData items makeRow config widgetId
    newWidget = makeDropdown widgetData items makeMain makeRow config newState
    newNode = node
      & L.widget .~ newWidget
      & L.children .~ Seq.fromList [mainNode, listViewNode]

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.dropdownStyle

  init wenv node = resultWidget $ createDropdown wenv node state

  merge wenv newNode oldNode oldState = result where
    result = resultWidget $ createDropdown wenv newNode oldState

  dispose wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    reqs = [ ResetOverlay widgetId | isOpen ]

  findNextFocus wenv node direction start
    | isOpen = node ^. L.children
    | otherwise = Empty

  findByPoint wenv node start point = result where
    children = node ^. L.children
    mainNode = Seq.index children mainIdx
    listNode = Seq.index children listIdx
    result
      | isOpen && isPointInNodeVp point listNode = Just listIdx
      | not isOpen && isPointInNodeVp point mainNode = Just mainIdx
      | otherwise = Nothing

  ddFocusChange evts reqs node = Just newResult where
    tmpResult = handleFocusChange evts reqs config node
    newResult = fromMaybe (resultWidget node) tmpResult
      & L.requests %~ (|> IgnoreChildrenEvents)

  handleEvent wenv node target evt = case evt of
    Focus
      | not isOpen -> ddFocusChange _ddcOnFocus _ddcOnFocusReq node
    Blur
      | not isOpen && not (seqStartsWith path focusedPath)
        -> ddFocusChange _ddcOnBlur _ddcOnBlurReq node
    Enter{} -> Just result where
      newIcon = fromMaybe CursorHand (style ^. L.cursorIcon)
      result = resultReqs node [SetCursorIcon widgetId CursorHand]
    Move point -> result where
      mainNode = Seq.index (node ^. L.children) mainIdx
      listNode = Seq.index (node ^. L.children) listIdx
      lvPoint = addPoint (negPoint (_ddsOffset state)) point
      validMainPos = isPointInNodeVp point mainNode
      validListPos = isOpen && isPointInNodeVp lvPoint listNode
      validPos = validMainPos || validListPos
      isArrow = Just CursorArrow == (snd <$> wenv ^. L.cursor)
      resetRes = resultReqs node [SetCursorIcon widgetId CursorArrow]
      result
        | not validPos && not isArrow = Just resetRes
        | otherwise = Nothing
    ButtonAction _ btn PressedBtn _
      | btn == wenv ^. L.mainButton && not isOpen -> result where
        result = Just $ resultReqs node [SetFocus (node ^. L.info . L.widgetId)]
    Click point _
      | openRequired point node -> Just $ openDropdown wenv node
      | closeRequired point node -> Just $ closeDropdown wenv node
    KeyAction mode code KeyPressed
      | isKeyOpenDropdown && not isOpen -> Just $ openDropdown wenv node
      | isKeyEscape code && isOpen -> Just $ closeDropdown wenv node
      where
        activationKeys = [isKeyDown, isKeyUp, isKeySpace, isKeyReturn]
        isKeyOpenDropdown = or (fmap ($ code) activationKeys)
    _
      | not isOpen -> Just $ resultReqs node [IgnoreChildrenEvents]
      | otherwise -> Nothing
    where
      style = activeStyle wenv node
      widgetId = node ^. L.info . L.widgetId
      path = node ^. L.info . L.path
      focusedPath = wenv ^. L.focusedPath

  openRequired point node = not isOpen && inViewport where
    inViewport = pointInRect point (node ^. L.info . L.viewport)

  closeRequired point node = isOpen && not inOverlay where
    offset = _ddsOffset state
    listNode = Seq.index (node ^. L.children) listIdx
    listVp = moveRect offset (listNode ^. L.info . L.viewport)
    inOverlay = pointInRect point listVp

  openDropdown wenv node = resultReqs newNode requests where
    newState = state {
      _ddsOpen = True,
      _ddsOffset = listOffset wenv node
    }
    newNode = node
      & L.widget .~ makeDropdown widgetData items makeMain makeRow config newState
    path = node ^. L.info . L.path
    widgetId = node ^. L.info . L.widgetId
    -- listView is wrapped by a scroll widget
    lvWid = node^?! L.children. ix listIdx. L.children. ix 0. L.info. L.widgetId
    requests = [SetOverlay widgetId path, SetFocus lvWid]

  closeDropdown wenv node = resultReqs newNode requests where
    widgetId = node ^. L.info . L.widgetId
    newState = state {
      _ddsOpen = False,
      _ddsOffset = def
    }
    newNode = node
      & L.widget .~ makeDropdown widgetData items makeMain makeRow config newState
    requests = [ResetOverlay widgetId, SetFocus widgetId]

  handleMessage wenv node target msg =
    cast msg >>= handleLvMsg wenv node

  handleLvMsg wenv node (OnChangeMessage idx) =
    Seq.lookup idx items >>= \value -> Just $ onChange wenv node idx value
  handleLvMsg wenv node OnListBlur = Just result where
    tempResult = closeDropdown wenv node
    result = tempResult & L.requests %~ (|> createMoveFocusReq wenv)

  onChange wenv node idx item = result where
    WidgetResult newNode reqs = closeDropdown wenv node
    newReqs = Seq.fromList $ widgetDataSet widgetData item
      ++ _ddcOnChangeReq config
      ++ fmap ($ idx) (_ddcOnChangeIdxReq config)
    evts = RaiseEvent <$> fmap ($ item) (_ddcOnChange config)
    evtsIdx = RaiseEvent <$> fmap (\fn -> fn idx item) (_ddcOnChangeIdx config)
    newEvents = Seq.fromList (evts ++ evtsIdx)
    result = WidgetResult newNode (reqs <> newReqs <> newEvents)

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = (newReqW, newReqH) where
    -- Main section reqs
    mainC = Seq.index children 0
    mainReqW = mainC ^. L.info . L.sizeReqW
    mainReqH = mainC ^. L.info . L.sizeReqH
    -- List items reqs
    listC = Seq.index children 1
    listReqW = listC ^. L.info . L.sizeReqW
    -- Items other than main could be wider
    -- Height only matters for the selected item, since the rest is in a scroll
    newReqW = sizeReqMergeMax mainReqW listReqW
    newReqH = mainReqH

  listHeight wenv node = maxHeight where
    Size _ winH = _weWindowSize wenv
    theme = activeTheme wenv node
    maxHeightTheme = theme ^. L.dropdownMaxHeight
    cfgMaxHeight = _ddcMaxHeight config
    -- Avoid having an invisible list if style/theme as not set
    maxHeightStyle = max 20 $ fromMaybe maxHeightTheme cfgMaxHeight
    reqHeight = case Seq.lookup 1 (node ^. L.children) of
      Just child -> sizeReqMaxBounded $ child ^. L.info . L.sizeReqH
      _ -> 0
    maxHeight = min winH (min reqHeight maxHeightStyle)

  listOffset wenv node = Point 0 newOffset where
    Size _ winH = _weWindowSize wenv
    viewport = node ^. L.info . L.viewport
    scOffset = wenv ^. L.offset
    Rect rx ry rw rh = moveRect scOffset viewport
    lh = listHeight wenv node
    newOffset
      | ry + rh + lh > winH = - (rh + lh)
      | otherwise = 0

  resize wenv node viewport children = resized where
    Rect rx ry rw rh = viewport
    !mainArea = viewport
    !listArea = viewport {
      _rY = ry + rh,
      _rH = listHeight wenv node
    }
    assignedAreas = Seq.fromList [mainArea, listArea]
    resized = (resultWidget node, assignedAreas)

  render wenv node renderer = do
    drawInScissor renderer True viewport $
      drawStyledAction renderer viewport style $ \contentArea -> do
        widgetRender (mainNode ^. L.widget) wenv mainNode renderer
        renderArrow renderer style contentArea

    when isOpen $
      createOverlay renderer $
        drawInTranslation renderer totalOffset $ do
          renderOverlay renderer cwenv listOverlay
    where
      style = activeStyle wenv node
      viewport = node ^. L.info . L.viewport
      mainNode = Seq.index (node ^. L.children) mainIdx
      -- List view is rendered with an offset to accommodate for window height
      listOverlay = Seq.index (node ^. L.children) listIdx
      listOverlayVp = listOverlay ^. L.info . L.viewport
      scOffset = wenv ^. L.offset
      offset = _ddsOffset state
      totalOffset = addPoint scOffset offset
      cwenv = updateWenvOffset container wenv node
        & L.viewport .~ listOverlayVp

  renderArrow renderer style contentArea =
    drawArrowDown renderer arrowRect (_sstFgColor style)
    where
      Rect x y w h = contentArea
      size = style ^. L.text . non def . L.fontSize . non def
      arrowW = unFontSize size / 2
      dh = (h - arrowW) / 2
      arrowRect = Rect (x + w - dh) (y + dh * 1.25) arrowW (arrowW / 2)

  renderOverlay renderer wenv overlayNode = renderAction where
    widget = overlayNode ^. L.widget
    renderAction = widgetRender widget wenv overlayNode renderer

makeListView
  :: (DropdownItem a, WidgetEvent e)
  => WidgetEnv s e
  -> WidgetData s a
  -> Seq a
  -> (a -> WidgetNode s e)
  -> DropdownCfg s e a
  -> WidgetId
  -> WidgetNode s e
makeListView wenv value items makeRow config widgetId = listViewNode where
  normalTheme = collectTheme wenv L.dropdownItemStyle
  selectedTheme = collectTheme wenv L.dropdownItemSelectedStyle
  itemStyle = fromJust (Just normalTheme <> _ddcItemStyle config)
  itemSelStyle = fromJust (Just selectedTheme <> _ddcItemSelectedStyle config)
  lvConfig = [
      selectOnBlur,
      onBlurReq (SendMessage widgetId OnListBlur),
      onChangeIdxReq (SendMessage widgetId . OnChangeMessage),
      itemNormalStyle itemStyle,
      itemSelectedStyle itemSelStyle
    ]
  lvStyle = collectTheme wenv L.dropdownListStyle
  listViewNode = listViewD_ value items makeRow lvConfig
    & L.info . L.style .~ lvStyle
    & L.info . L.overlay .~ True

createMoveFocusReq :: WidgetEnv s e -> WidgetRequest s e
createMoveFocusReq wenv = MoveFocus Nothing direction where
  direction
    | wenv ^. L.inputStatus . L.keyMod . L.leftShift = FocusBwd
    | otherwise = FocusFwd
