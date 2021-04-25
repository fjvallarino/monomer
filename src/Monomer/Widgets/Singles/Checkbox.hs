{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Singles.Checkbox (
  CheckboxMark(..),
  checkbox,
  checkbox_,
  checkboxV,
  checkboxV_,
  checkboxD_,
  checkboxMark
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data CheckboxMark
  = CheckboxSquare
  | CheckboxTimes
  deriving (Eq, Show)

data CheckboxCfg s e = CheckboxCfg {
  _ckcMark :: Maybe CheckboxMark,
  _ckcWidth :: Maybe Double,
  _ckcOnFocus :: [e],
  _ckcOnFocusReq :: [WidgetRequest s e],
  _ckcOnBlur :: [e],
  _ckcOnBlurReq :: [WidgetRequest s e],
  _ckcOnChange :: [Bool -> e],
  _ckcOnChangeReq :: [WidgetRequest s e]
}

instance Default (CheckboxCfg s e) where
  def = CheckboxCfg {
    _ckcMark = Nothing,
    _ckcWidth = Nothing,
    _ckcOnFocus = [],
    _ckcOnFocusReq = [],
    _ckcOnBlur = [],
    _ckcOnBlurReq = [],
    _ckcOnChange = [],
    _ckcOnChangeReq = []
  }

instance Semigroup (CheckboxCfg s e) where
  (<>) t1 t2 = CheckboxCfg {
    _ckcMark = _ckcMark t2 <|> _ckcMark t1,
    _ckcWidth = _ckcWidth t2 <|> _ckcWidth t1,
    _ckcOnFocus = _ckcOnFocus t1 <> _ckcOnFocus t2,
    _ckcOnFocusReq = _ckcOnFocusReq t1 <> _ckcOnFocusReq t2,
    _ckcOnBlur = _ckcOnBlur t1 <> _ckcOnBlur t2,
    _ckcOnBlurReq = _ckcOnBlurReq t1 <> _ckcOnBlurReq t2,
    _ckcOnChange = _ckcOnChange t1 <> _ckcOnChange t2,
    _ckcOnChangeReq = _ckcOnChangeReq t1 <> _ckcOnChangeReq t2
  }

instance Monoid (CheckboxCfg s e) where
  mempty = def

instance CmbWidth (CheckboxCfg s e) where
  width w = def {
    _ckcWidth = Just w
  }

instance CmbOnFocus (CheckboxCfg s e) e where
  onFocus fn = def {
    _ckcOnFocus = [fn]
  }

instance CmbOnFocusReq (CheckboxCfg s e) s e where
  onFocusReq req = def {
    _ckcOnFocusReq = [req]
  }

instance CmbOnBlur (CheckboxCfg s e) e where
  onBlur fn = def {
    _ckcOnBlur = [fn]
  }

instance CmbOnBlurReq (CheckboxCfg s e) s e where
  onBlurReq req = def {
    _ckcOnBlurReq = [req]
  }

instance CmbOnChange (CheckboxCfg s e) Bool e where
  onChange fn = def {
    _ckcOnChange = [fn]
  }

instance CmbOnChangeReq (CheckboxCfg s e) s e where
  onChangeReq req = def {
    _ckcOnChangeReq = [req]
  }

checkboxMark :: CheckboxMark -> CheckboxCfg s e
checkboxMark mark = def {
  _ckcMark = Just mark
}

checkbox :: WidgetEvent e => ALens' s Bool -> WidgetNode s e
checkbox field = checkbox_ field def

checkbox_
  :: WidgetEvent e => ALens' s Bool -> [CheckboxCfg s e] -> WidgetNode s e
checkbox_ field config = checkboxD_ (WidgetLens field) config

checkboxV :: WidgetEvent e => Bool -> (Bool -> e) -> WidgetNode s e
checkboxV value handler = checkboxV_ value handler def

checkboxV_
  :: WidgetEvent e => Bool -> (Bool -> e) -> [CheckboxCfg s e] -> WidgetNode s e
checkboxV_ value handler config = checkboxD_ (WidgetValue value) newConfig where
  newConfig = onChange handler : config

checkboxD_
  :: WidgetEvent e => WidgetData s Bool -> [CheckboxCfg s e] -> WidgetNode s e
checkboxD_ widgetData configs = checkboxNode where
  config = mconcat configs
  widget = makeCheckbox widgetData config
  checkboxNode = defaultWidgetNode "checkbox" widget
    & L.info . L.focusable .~ True

makeCheckbox
  :: WidgetEvent e => WidgetData s Bool -> CheckboxCfg s e -> Widget s e
makeCheckbox widgetData config = widget where
  widget = createSingle () def {
    singleGetBaseStyle = getBaseStyle,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.checkboxStyle

  handleEvent wenv node target evt = case evt of
    Focus -> handleFocusChange _ckcOnFocus _ckcOnFocusReq config node
    Blur -> handleFocusChange _ckcOnBlur _ckcOnBlurReq config node
    Click p _
      | isPointInNodeVp p node -> Just $ resultReqsEvts node reqs events
    KeyAction mod code KeyPressed
      | isSelectKey code -> Just $ resultReqsEvts node reqs events
    _ -> Nothing
    where
      isSelectKey code = isKeyReturn code || isKeySpace code
      model = _weModel wenv
      path = node ^. L.info . L.path
      value = widgetDataGet model widgetData
      newValue = not value
      events = fmap ($ newValue) (_ckcOnChange config)
      setValueReq = widgetDataSet widgetData newValue
      reqs = setValueReq ++ _ckcOnChangeReq config

  getSizeReq wenv node currState = req where
    theme = activeTheme wenv node
    width = fromMaybe (theme ^. L.checkboxWidth) (_ckcWidth config)
    req = (fixedSize width, fixedSize width)

  render wenv node renderer = do
    renderCheckbox renderer checkboxBW checkboxArea fgColor

    when value $
      renderMark renderer checkboxBW checkboxArea fgColor mark
    where
      model = _weModel wenv
      theme = activeTheme wenv node
      style = activeStyle wenv node
      value = widgetDataGet model widgetData
      carea = getContentArea style node
      checkboxW = fromMaybe (theme ^. L.checkboxWidth) (_ckcWidth config)
      checkboxBW = max 1 (checkboxW * 0.1)
      checkboxL = _rX carea + (_rW carea - checkboxW) / 2
      checkboxT = _rY carea + (_rH carea - checkboxW) / 2
      checkboxArea = Rect checkboxL checkboxT checkboxW checkboxW
      fgColor = styleFgColor style
      mark = fromMaybe CheckboxSquare (_ckcMark config)

renderCheckbox :: Renderer -> Double -> Rect -> Color -> IO ()
renderCheckbox renderer checkboxBW rect color = action where
  side = Just $ BorderSide checkboxBW color
  border = Border side side side side
  action = drawRectBorder renderer rect border Nothing

renderMark :: Renderer -> Double -> Rect -> Color -> CheckboxMark -> IO ()
renderMark renderer checkboxBW rect color mark = action where
  w = checkboxBW * 2
  lw = checkboxBW * 2
  newRect = fromMaybe def (subtractFromRect rect w w w w)
  action
    | mark == CheckboxSquare = drawRect renderer newRect (Just color) Nothing
    | otherwise = drawTimesX renderer newRect lw (Just color)
