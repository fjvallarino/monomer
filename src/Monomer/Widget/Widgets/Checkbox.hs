module Monomer.Widget.Widgets.Checkbox (
  CheckboxCfg(..),
  checkbox,
  checkboxCfg
) where

import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.StyleUtil (removeOuterBounds)
import Monomer.Common.Tree
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Color
import Monomer.Graphics.Drawing
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.BaseSingle
import Monomer.Widget.Types
import Monomer.Widget.Util

data CheckboxCfg s e = CheckboxCfg {
  _ckcValue :: WidgetValue s Bool,
  _ckcOnChange :: [Bool -> e],
  _ckcOnChangeReq :: [WidgetRequest s],
  _ckcWidth :: Double,
  _ckcSize :: Double
}

checkboxCfg :: WidgetValue s Bool -> CheckboxCfg s e
checkboxCfg value = CheckboxCfg {
  _ckcValue = value,
  _ckcOnChange = [],
  _ckcOnChangeReq = [],
  _ckcWidth = 2,
  _ckcSize = 25
}

checkbox :: ALens' s Bool -> WidgetInstance s e
checkbox field = checkboxInstance where
  config = checkboxCfg (WidgetLens field)
  checkboxInstance = (defaultWidgetInstance "checkbox" (makeCheckbox config)) {
    _wiFocusable = True
  }

makeCheckbox :: CheckboxCfg s e -> Widget s e
makeCheckbox config = widget where
  widget = createSingle def {
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  handleEvent wenv target evt inst = case evt of
    Click (Point x y) _ -> Just $ resultReqs (setFocusReq : setValueReq) inst
    KeyAction mod code KeyPressed
      | isSelectKey code -> Just $ resultReqs setValueReq inst
    _ -> Nothing
    where
      isSelectKey code = isKeyReturn code || isKeySpace code
      model = _weModel wenv
      value = widgetValueGet model (_ckcValue config)
      setValueReq = widgetValueSet (_ckcValue config) (not value)
      setFocusReq = SetFocus $ _wiPath inst

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    sz = _ckcSize config
    size = Size sz sz
    sizeReq = SizeReq size StrictSize StrictSize

  render renderer wenv inst = do
    drawStyledBackground renderer rarea style
    renderCheckbox renderer config rarea fgColor

    when value $
      renderMark renderer config rarea fgColor
    where
      model = _weModel wenv
      style = activeStyle wenv inst
      value = widgetValueGet model (_ckcValue config)
      rarea = removeOuterBounds style $ _wiRenderArea inst
      checkboxL = _rX rarea
      checkboxT = _rY rarea
      sz = min (_rW rarea) (_rH rarea)
      checkboxArea = Rect checkboxL checkboxT sz sz
      fgColor = activeFgColor wenv inst

renderCheckbox
  :: (Monad m) => Renderer m -> CheckboxCfg s e -> Rect -> Color -> m ()
renderCheckbox renderer config rect color = action where
  width = _ckcWidth config
  action = drawRectBorder renderer rect border Nothing
  side = Just $ BorderSide 2 color
  border = Border side side side side

renderMark
  :: (Monad m) => Renderer m -> CheckboxCfg s e -> Rect -> Color -> m ()
renderMark renderer config rect color = action where
  w = _ckcWidth config * 2
  newRect = subtractFromRect rect w w w w
  action = drawRect renderer newRect (Just color) Nothing
