{-# LANGUAGE BangPatterns #-}
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
import Control.Lens (ALens', (&), (^.), (^?), (.~), (%~), (<>~), _Just, non)
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
  _ddcOnFocus :: [e],
  _ddcOnFocusReq :: [WidgetRequest s],
  _ddcOnBlur :: [e],
  _ddcOnBlurReq :: [WidgetRequest s],
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

instance CmbOnFocusReq (DropdownCfg s e a) s where
  onFocusReq req = def {
    _ddcOnFocusReq = [req]
  }

instance CmbOnBlur (DropdownCfg s e a) e where
  onBlur fn = def {
    _ddcOnBlur = [fn]
  }

instance CmbOnBlurReq (DropdownCfg s e a) s where
  onBlurReq req = def {
    _ddcOnBlurReq = [req]
  }

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
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> WidgetNode s e
dropdown field items makeMain makeRow = newNode where
  newNode = dropdown_ field items makeMain makeRow def

dropdown_
  :: (Traversable t, DropdownItem a)
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
  :: (Traversable t, DropdownItem a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> WidgetNode s e
dropdownV value handler items makeMain makeRow = newNode where
  newNode = dropdownV_ value handler items makeMain makeRow def

dropdownV_
  :: (Traversable t, DropdownItem a)
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
  :: (Traversable t, DropdownItem a)
  => WidgetData s a
  -> t a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> [DropdownCfg s e a]
  -> WidgetNode s e
dropdownD_ widgetData items makeMain makeRow configs = makeNode widget where
  config = mconcat configs
  newState = DropdownState False
  newItems = foldl' (|>) Empty items
  widget = makeDropdown widgetData newItems makeMain makeRow config newState

makeNode :: Widget s e -> WidgetNode s e
makeNode widget = defaultWidgetNode "dropdown" widget
  & L.info . L.focusable .~ True

makeDropdown
  :: DropdownItem a
  => WidgetData s a
  -> Seq a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
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

  createDropdown wenv newState node = newNode where
    selected = currentValue wenv
    mainStyle = collectTheme wenv L.dropdownStyle
    mainNode = makeMain selected
      & L.info . L.style .~ mainStyle
    path = node ^. L.info . L.path
    listViewNode = makeListView wenv widgetData items makeRow config path
    newWidget = makeDropdown widgetData items makeMain makeRow config newState
    newNode = node
      & L.widget .~ newWidget
      & L.children .~ Seq.fromList [mainNode, listViewNode]

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.dropdownStyle

  init wenv node = resultWidget $ createDropdown wenv state node

  merge wenv oldState oldNode newNode = result where
    newState = fromMaybe state (useState oldState)
    result = resultWidget $ createDropdown wenv newState newNode

  findNextFocus wenv direction start node
    | isOpen = node ^. L.children
    | otherwise = Empty

  ddFocusChange evts reqs node = Just newResult where
    tmpResult = handleFocusChange evts reqs config node
    newResult = fromMaybe (resultWidget node) tmpResult
      & L.requests %~ (|> IgnoreChildrenEvents)

  handleEvent wenv target evt node = case evt of
    Focus
      | not isOpen -> ddFocusChange _ddcOnFocus _ddcOnFocusReq node
    Blur
      | not isOpen && not (seqStartsWith path focusedPath)
        -> ddFocusChange _ddcOnBlur _ddcOnBlurReq node
    Enter{} -> Nothing -- to have handleStyleChange applied
    ButtonAction _ btn PressedBtn _
      | btn == wenv ^. L.mainButton && not isOpen -> result where
        result = Just $ resultReqs node [SetFocus (node ^. L.info . L.path)]
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
      path = node ^. L.info . L.path
      focusedPath = wenv ^. L.focusedPath

  openRequired point node = not isOpen && inViewport where
    inViewport = pointInRect point (node ^. L.info . L.viewport)

  closeRequired point node = isOpen && not inOverlay where
    inOverlay = case Seq.lookup listIdx (node ^. L.children) of
      Just node -> pointInRect point (node ^. L.info . L.viewport)
      Nothing -> False

  openDropdown wenv node = resultReqs newNode requests where
    newState = DropdownState True
    newNode = node
      & L.widget .~ makeDropdown widgetData items makeMain makeRow config newState
    path = node ^. L.info . L.path
    -- listView is wrapped by a scroll widget
    lvPath = path |> listIdx |> 0
    requests = [SetOverlay path, SetFocus lvPath]

  closeDropdown wenv node = resultReqs newNode requests where
    path = node ^. L.info . L.path
    newState = DropdownState False
    newNode = node
      & L.widget .~ makeDropdown widgetData items makeMain makeRow config newState
    requests = [ResetOverlay, SetFocus path]

  handleMessage wenv target msg node =
    cast msg >>= handleLvMsg wenv node

  handleLvMsg wenv node (OnChangeMessage idx) =
    Seq.lookup idx items >>= \value -> Just $ onChange wenv idx value node
  handleLvMsg wenv node OnListBlur = Just result where
    tempResult = closeDropdown wenv node
    result = tempResult & L.requests %~ (|> createMoveFocusReq wenv)

  onChange wenv idx item node = result where
    WidgetResult newNode reqs events = closeDropdown wenv node
    newReqs = Seq.fromList $ widgetDataSet widgetData item
      ++ _ddcOnChangeReq config
      ++ fmap ($ idx) (_ddcOnChangeIdxReq config)
    newEvents = Seq.fromList $ fmap ($ item) (_ddcOnChange config)
      ++ fmap (\fn -> fn idx item) (_ddcOnChangeIdx config)
    result = WidgetResult newNode (reqs <> newReqs) (events <> newEvents)

  getSizeReq wenv node children = (newReqW, newReqH) where
    -- Main section reqs
    mainC = Seq.index children 0
    mainReq = widgetUpdateSizeReq (mainC ^. L.widget) wenv mainC
    mainReqW = mainReq ^. L.info . L.sizeReqW
    mainReqH = mainReq ^. L.info . L.sizeReqH
    -- List items reqs
    listC = Seq.index children 1
    listReq = widgetUpdateSizeReq (listC ^. L.widget) wenv listC
    listReqW = listReq ^. L.info . L.sizeReqW
    -- Items other than main could be wider
    -- Height only matters for the selected item, since the rest is in a scroll
    newReqW = sizeReqMergeMax mainReqW listReqW
    newReqH = mainReqH

  resize wenv viewport renderArea children node = resized where
    Size winW winH = _weWindowSize wenv
    Rect rx ry rw rh = renderArea
    theme = activeTheme wenv node
    dropdownY dh
      | ry + rh + dh <= winH = ry + rh
      | ry - dh >= 0 = ry - dh
      | otherwise = 0
    !listArea = case Seq.lookup 1 children of
      Just child -> (oViewport, oRenderArea) where
        maxHeightTheme = theme ^. L.dropdownMaxHeight
        cfgMaxHeight = _ddcMaxHeight config
        -- Avoid having an invisible list if style/theme as not set
        maxHeightStyle = max 20 $ fromMaybe maxHeightTheme cfgMaxHeight
        reqHeight = sizeReqMin $ child ^. L.info . L.sizeReqH
        maxHeight = min winH (min reqHeight maxHeightStyle)
        dy = dropdownY maxHeight
        dh = maxHeight
        !oViewport = viewport {
          _rY = dy,
          _rH = dh
        }
        !oRenderArea = renderArea {
          _rY = dy,
          _rH = dh
        }
      Nothing -> (viewport, renderArea)
    !mainArea = (viewport, renderArea)
    assignedAreas = Seq.fromList [mainArea, listArea]
    resized = (node, assignedAreas)

  render renderer wenv node = do
    drawInScissor renderer True viewport $
      drawStyledAction renderer renderArea style $ \contentArea -> do
        widgetRender (mainNode ^. L.widget) renderer wenv mainNode
        renderArrow renderer style contentArea

    when (isOpen && isJust listViewOverlay) $
      createOverlay renderer $
        renderOverlay renderer wenv (fromJust listViewOverlay)
    where
      style = activeStyle wenv node
      viewport = node ^. L.info . L.viewport
      renderArea = node ^. L.info . L.renderArea
      mainNode = Seq.index (node ^. L.children) mainIdx
      listViewOverlay = Seq.lookup listIdx (node ^. L.children)

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
    renderAction = widgetRender widget renderer wenv overlayNode

makeListView
  :: DropdownItem a
  => WidgetEnv s e
  -> WidgetData s a
  -> Seq a
  -> (a -> WidgetNode s e)
  -> DropdownCfg s e a
  -> Path
  -> WidgetNode s e
makeListView wenv value items makeRow config path = listViewNode where
  normalTheme = collectTheme wenv L.dropdownItemStyle
  selectedTheme = collectTheme wenv L.dropdownItemSelectedStyle
  itemStyle = fromJust (Just normalTheme <> _ddcItemStyle config)
  itemSelStyle = fromJust (Just selectedTheme <> _ddcItemSelectedStyle config)
  lvConfig = [
      selectOnBlur True,
      onBlurReq (SendMessage path OnListBlur),
      onChangeIdxReq (SendMessage path . OnChangeMessage),
      itemNormalStyle itemStyle,
      itemSelectedStyle itemSelStyle
    ]
  lvStyle = collectTheme wenv L.dropdownListStyle
  listViewNode = listViewD_ value items makeRow lvConfig
    & L.info . L.style .~ lvStyle

createMoveFocusReq :: WidgetEnv s e -> WidgetRequest s
createMoveFocusReq wenv = MoveFocus Nothing direction where
  direction
    | wenv ^. L.inputStatus . L.keyMod . L.leftShift = FocusBwd
    | otherwise = FocusFwd
