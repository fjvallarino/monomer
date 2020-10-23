{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widgets.ListView (
  ListViewCfg,
  listView,
  listView_,
  listViewV,
  listViewV_,
  listViewD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (.~))
import Control.Monad (when)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Monomer.Graphics.Lens
import Monomer.Widgets.Box
import Monomer.Widgets.Container
import Monomer.Widgets.Label
import Monomer.Widgets.Scroll
import Monomer.Widgets.Spacer
import Monomer.Widgets.Stack

import qualified Monomer.Core.Lens as L

data ListViewCfg s e a = ListViewCfg {
  _lvcOnBlur :: [e],
  _lvcOnBlurReq :: [WidgetRequest s],
  _lvcOnChange :: [a -> e],
  _lvcOnChangeReq :: [WidgetRequest s],
  _lvcOnChangeIdx :: [Int -> a -> e],
  _lvcOnChangeIdxReq :: [Int -> WidgetRequest s],
  _lvcSelectedOnBlur :: Maybe Bool,
  _lvcSelectedStyle :: Maybe StyleState,
  _lvcHoverStyle :: Maybe StyleState,
  _lvcHighlightedColor :: Maybe Color
}

instance Default (ListViewCfg s e a) where
  def = ListViewCfg {
    _lvcOnBlur = [],
    _lvcOnBlurReq = [],
    _lvcOnChange = [],
    _lvcOnChangeReq = [],
    _lvcOnChangeIdx = [],
    _lvcOnChangeIdxReq = [],
    _lvcSelectedOnBlur = Nothing,
    _lvcSelectedStyle = Just $ bgColor gray,
    _lvcHoverStyle = Just $ bgColor darkGray,
    _lvcHighlightedColor = Just lightGray
  }

instance Semigroup (ListViewCfg s e a) where
  (<>) t1 t2 = ListViewCfg {
    _lvcOnBlur = _lvcOnBlur t1 <> _lvcOnBlur t2,
    _lvcOnBlurReq = _lvcOnBlurReq t1 <> _lvcOnBlurReq t2,
    _lvcOnChange = _lvcOnChange t1 <> _lvcOnChange t2,
    _lvcOnChangeReq = _lvcOnChangeReq t1 <> _lvcOnChangeReq t2,
    _lvcOnChangeIdx = _lvcOnChangeIdx t1 <> _lvcOnChangeIdx t2,
    _lvcOnChangeIdxReq = _lvcOnChangeIdxReq t1 <> _lvcOnChangeIdxReq t2,
    _lvcSelectedOnBlur = _lvcSelectedOnBlur t2 <|> _lvcSelectedOnBlur t1,
    _lvcSelectedStyle = _lvcSelectedStyle t2 <|> _lvcSelectedStyle t1,
    _lvcHoverStyle = _lvcHoverStyle t2 <|> _lvcHoverStyle t1,
    _lvcHighlightedColor = _lvcHighlightedColor t2 <|> _lvcHighlightedColor t1
  }

instance Monoid (ListViewCfg s e a) where
  mempty = ListViewCfg {
    _lvcOnBlur = [],
    _lvcOnBlurReq = [],
    _lvcOnChange = [],
    _lvcOnChangeReq = [],
    _lvcOnChangeIdx = [],
    _lvcOnChangeIdxReq = [],
    _lvcSelectedOnBlur = Nothing,
    _lvcSelectedStyle = Nothing,
    _lvcHoverStyle = Nothing,
    _lvcHighlightedColor = Nothing
  }

instance OnBlur (ListViewCfg s e a) e where
  onBlur fn = def {
    _lvcOnBlur = [fn]
  }

instance OnBlurReq (ListViewCfg s e a) s where
  onBlurReq req = def {
    _lvcOnBlurReq = [req]
  }

instance OnChange (ListViewCfg s e a) a e where
  onChange fn = def {
    _lvcOnChange = [fn]
  }

instance OnChangeReq (ListViewCfg s e a) s where
  onChangeReq req = def {
    _lvcOnChangeReq = [req]
  }

instance OnChangeIdx (ListViewCfg s e a) a e where
  onChangeIdx fn = def {
    _lvcOnChangeIdx = [fn]
  }

instance OnChangeIdxReq (ListViewCfg s e a) s where
  onChangeIdxReq req = def {
    _lvcOnChangeIdxReq = [req]
  }

instance SelectOnBlur (ListViewCfg s e a) where
  selectOnBlur select = def {
    _lvcSelectedOnBlur = Just select
  }

instance SelectedStyle (ListViewCfg s e a) where
  selectedStyle style = def {
    _lvcSelectedStyle = Just style
  }

instance HoverStyle (ListViewCfg s e a) where
  hoverStyle style = def {
    _lvcHoverStyle = Just style
  }

instance HighlightedColor (ListViewCfg s e a) where
  highlightedColor color = def {
    _lvcHighlightedColor = Just color
  }

newtype ListViewState = ListViewState {
  _highlighted :: Int
}

newtype ListViewMessage
  = OnClickMessage Int
  deriving Typeable

listView
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> WidgetInstance s e
listView field items makeRow = listView_ field items makeRow def

listView_
  :: (Traversable t, Eq a)
  => ALens' s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listView_ field items makeRow configs = newInst where
  newInst = listViewD_ (WidgetLens field) items makeRow configs

listViewV
  :: (Traversable t, Eq a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> (a -> WidgetInstance s e)
  -> WidgetInstance s e
listViewV value handler items makeRow = newInst where
  newInst = listViewV_ value handler items makeRow def

listViewV_
  :: (Traversable t, Eq a)
  => a
  -> (Int -> a -> e)
  -> t a
  -> (a -> WidgetInstance s e)
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listViewV_ value handler items makeRow configs = newInst where
  widgetData = WidgetValue value
  newConfigs = onChangeIdx handler : configs
  newInst = listViewD_ widgetData items makeRow newConfigs

listViewD_
  :: (Traversable t, Eq a)
  => WidgetData s a
  -> t a
  -> (a -> WidgetInstance s e)
  -> [ListViewCfg s e a]
  -> WidgetInstance s e
listViewD_ widgetData items makeRow configs = makeInstance widget where
  config = mconcat configs
  newItems = foldl' (|>) Empty items
  newState = ListViewState 0
  widget = makeListView widgetData newItems makeRow config newState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = scroll $ (defaultWidgetInstance "listView" widget) {
  _wiFocusable = True
}

makeListView
  :: (Eq a)
  => WidgetData s a
  -> Seq a
  -> (a -> WidgetInstance s e)
  -> ListViewCfg s e a
  -> ListViewState
  -> Widget s e
makeListView widgetData items makeRow config state = widget where
  baseWidget = createContainer def {
    containerInit = init,
    containerGetState = makeState state,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  widget = baseWidget {
    widgetRender = render
  }

  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createListView wenv newState inst = newInstance where
    selected = currentValue wenv
    highlighted = _highlighted newState
    path = _wiPath inst
    itemsList = makeItemsList items makeRow config path selected highlighted
    newInstance = inst {
      _wiWidget = makeListView widgetData items makeRow config newState,
      _wiChildren = Seq.singleton itemsList
    }

  init wenv inst = resultWidget $ createListView wenv state inst

  merge wenv oldState newInstance = result where
    newState = fromMaybe state (useState oldState)
    result = resultWidget $ createListView wenv newState newInstance

  handleEvent wenv target evt inst = case evt of
    Blur -> result where
      isTabPressed = getKeyStatus (_weInputStatus wenv) keyTab == KeyPressed
      changeReq = isTabPressed && Just True == _lvcSelectedOnBlur config
      WidgetResult tempReqs tempEvts tempInst
        | changeReq = selectItem wenv inst (_highlighted state)
        | otherwise = resultWidget inst
      evts = tempEvts <> Seq.fromList (_lvcOnBlur config)
      reqs = tempReqs <> Seq.fromList (_lvcOnBlurReq config)
      mergedResult = Just $ WidgetResult reqs evts tempInst
      result
        | changeReq || not (null evts && null reqs) = mergedResult
        | otherwise = Nothing
    KeyAction mode code status
      | isKeyDown code && status == KeyPressed -> highlightNext wenv inst
      | isKeyUp code && status == KeyPressed -> highlightPrev wenv inst
      | isSelectKey code && status == KeyPressed -> resultSelected
      where
        resultSelected = Just $ selectItem wenv inst (_highlighted state)
        isSelectKey code = isKeyReturn code || isKeySpace code
    _ -> Nothing

  highlightNext wenv inst = highlightItem wenv inst nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx < length items - 1 = tempIdx + 1
      | otherwise = tempIdx

  highlightPrev wenv inst = highlightItem wenv inst nextIdx where
    tempIdx = _highlighted state
    nextIdx
      | tempIdx > 0 = tempIdx - 1
      | otherwise = tempIdx

  handleMessage wenv target message inst = result where
    handleSelect (OnClickMessage idx) = selectItem wenv inst idx
    result = fmap handleSelect (cast message)

  highlightItem wenv inst nextIdx = result where
    newState = ListViewState nextIdx
    newWidget = makeListView widgetData items makeRow config newState
    -- ListView's merge uses the old widget's state. Since we want the newly
    -- created state, the old widget is replaced here
    oldInstance = inst {
      _wiWidget = newWidget
    }
    -- ListView's tree will be rebuilt in merge, before merging its children,
    -- so it does not matter what we currently have
    newInstance = oldInstance
    widgetResult = widgetMerge newWidget wenv oldInstance newInstance
    scrollToReq = itemScrollTo inst nextIdx
    requests = Seq.fromList scrollToReq
    result = Just $ widgetResult {
      _wrRequests = requests,
      _wrWidget = resizeInstance wenv (_wrWidget widgetResult)
    }

  selectItem wenv inst idx = result where
    selected = currentValue wenv
    value = fromMaybe selected (Seq.lookup idx items)
    valueSetReq = widgetDataSet widgetData value
    scrollToReq = itemScrollTo inst idx
    events = fmap ($ value) (_lvcOnChange config)
      ++ fmap (\fn -> fn idx value) (_lvcOnChangeIdx config)
    changeReqs = _lvcOnChangeReq config
      ++ fmap ($ idx) (_lvcOnChangeIdxReq config)
    focusReq = [SetFocus $ _wiPath inst]
    requests = valueSetReq ++ scrollToReq ++ changeReqs ++ focusReq
    newState = ListViewState idx
    newInstance = inst {
      _wiWidget = makeListView widgetData items makeRow config newState
    }
    result = resultReqsEvents requests events newInstance

  itemScrollTo inst idx = maybeToList (fmap scrollReq renderArea) where
    renderArea = itemRenderArea inst idx
    scrollPath =  parentPath inst
    scrollReq rect = SendMessage scrollPath (ScrollTo rect)

  itemRenderArea inst idx = renderArea where
    lookup idx inst = Seq.lookup idx (_wiChildren inst)
    renderArea = fmap _wiRenderArea $ pure inst
      >>= lookup 0 -- vstack
      >>= lookup idx -- item

  getSizeReq wenv inst children = (newSizeReqW, newSizeReqH) where
    child = Seq.index children 0
    newSizeReqW = _wiSizeReqW child
    newSizeReqH = _wiSizeReqH child

  resize wenv viewport renderArea children inst = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (inst, assignedArea)

  render renderer wenv inst = do
    renderWrapper defaultRender renderer wenv inst

    when (isJust itemRect && isJust baseColor) $
      drawRectBorder renderer (fromJust itemRect) itemBorder Nothing
    where
      children = _wiChildren inst
      itemIdx = _highlighted state
      itemRect = itemRenderArea inst itemIdx
      baseColor = _lvcHighlightedColor config
      highlightColor
        | isFocused wenv inst = fromJust baseColor
        | otherwise = fromJust baseColor & a .~ 0.4
      bs = Just $ BorderSide 1 highlightColor
      itemBorder = Border bs bs bs bs

makeItemsList
  :: (Eq a)
  => Seq a
  -> (a -> WidgetInstance s e)
  -> ListViewCfg s e a
  -> Path
  -> a
  -> Int
  -> WidgetInstance s e
makeItemsList items makeRow config path selected hlIdx = itemsList where
  ListViewCfg{..} = config
  isSelected item = item == selected
  selectedStyle item
    | isSelected item = _lvcSelectedStyle
    | otherwise = Nothing
  itemStyle idx item = def
    & L.basic .~ selectedStyle item
    & L.hover .~ _lvcHoverStyle
  makeItem idx item = newItem where
    clickCfg = onClickReq $ SendMessage path (OnClickMessage idx)
    itemCfg = [expandContent, clickCfg]
    content = makeRow item
    newItem = box_ (content & L.style .~ itemStyle idx item) itemCfg
  pairs = Seq.zip (Seq.fromList [0..length items]) items
  itemsList = vstack $ fmap (uncurry makeItem) pairs

resizeInstance :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
resizeInstance wenv inst = newInst where
  viewport = _wiViewport inst
  renderArea = _wiRenderArea inst
  instReqs = widgetUpdateSizeReq (_wiWidget inst) wenv inst
  newInst = widgetResize (_wiWidget instReqs) wenv viewport renderArea instReqs
