{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widget.Widgets.Checkbox (
  CheckboxCfg(..),
  checkbox,
  checkbox_
) where

import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.StyleUtil (removeOuterBounds)
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.WidgetCombinators

data CheckboxCfg s e = CheckboxCfg {
  _ckcOnChange :: [Bool -> e],
  _ckcOnChangeReq :: [WidgetRequest s]
}

instance Default (CheckboxCfg s e) where
  def = CheckboxCfg {
    _ckcOnChange = [],
    _ckcOnChangeReq = []
  }

instance Semigroup (CheckboxCfg s e) where
  (<>) t1 t2 = CheckboxCfg {
    _ckcOnChange = _ckcOnChange t1 <> _ckcOnChange t2,
    _ckcOnChangeReq = _ckcOnChangeReq t1 <> _ckcOnChangeReq t2
  }

instance Monoid (CheckboxCfg s e) where
  mempty = def

instance OnChange (CheckboxCfg s e) Bool e where
  onChange fn = def {
    _ckcOnChange = [fn]
  }

instance OnChangeReq (CheckboxCfg s e) s where
  onChangeReq req = def {
    _ckcOnChangeReq = [req]
  }

checkboxWidth :: Double
checkboxWidth = 25

checkboxBorderW :: Double
checkboxBorderW = 2

checkbox :: ALens' s Bool -> WidgetInstance s e
checkbox field = checkbox_ field def

checkbox_ :: ALens' s Bool -> CheckboxCfg s e -> WidgetInstance s e
checkbox_ field config = checkboxInstance where
  widget = makeCheckbox (WidgetLens field) config
  checkboxInstance = (defaultWidgetInstance "checkbox" widget) {
    _wiFocusable = True
  }

makeCheckbox :: WidgetValue s Bool -> CheckboxCfg s e -> Widget s e
makeCheckbox field config = widget where
  widget = createSingle def {
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  handleEvent wenv target evt inst = case evt of
    Click (Point x y) _ -> Just $ resultReqsEvents clickReqs events inst
    KeyAction mod code KeyPressed
      | isSelectKey code -> Just $ resultReqsEvents reqs events inst
    _ -> Nothing
    where
      isSelectKey code = isKeyReturn code || isKeySpace code
      model = _weModel wenv
      value = widgetValueGet model field
      newValue = not value
      events = fmap ($ newValue) (_ckcOnChange config)
      setValueReq = widgetValueSet field newValue
      setFocusReq = SetFocus $ _wiPath inst
      reqs = setValueReq ++ _ckcOnChangeReq config
      clickReqs = setFocusReq : reqs

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    sz = checkboxWidth
    size = Size sz sz
    sizeReq = SizeReq size StrictSize StrictSize

  render renderer wenv inst = do
    renderCheckbox renderer config rarea fgColor

    when value $
      renderMark renderer config rarea fgColor
    where
      model = _weModel wenv
      style = activeStyle wenv inst
      value = widgetValueGet model field
      rarea = removeOuterBounds style $ _wiRenderArea inst
      checkboxL = _rX rarea
      checkboxT = _rY rarea
      sz = min (_rW rarea) (_rH rarea)
      checkboxArea = Rect checkboxL checkboxT sz sz
      fgColor = instanceFgColor wenv inst

renderCheckbox :: Renderer -> CheckboxCfg s e -> Rect -> Color -> IO ()
renderCheckbox renderer config rect color = action where
  side = Just $ BorderSide checkboxBorderW color
  border = Border side side side side
  action = drawRectBorder renderer rect border Nothing

renderMark :: Renderer -> CheckboxCfg s e -> Rect -> Color -> IO ()
renderMark renderer config rect color = action where
  w = checkboxBorderW * 2
  newRect = subtractFromRect rect w w w w
  action = drawRect renderer newRect (Just color) Nothing
