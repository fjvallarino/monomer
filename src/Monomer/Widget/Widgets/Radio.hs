module Monomer.Widget.Widgets.Radio (
  RadioCfg(..),
  radio,
  radioCfg
) where

import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Style
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

data RadioCfg s e a = RadioCfg {
  _rdcValue :: WidgetValue s a,
  _rdcOption :: a,
  _rdcOnChange :: [a -> e],
  _rdcOnChangeReq :: [WidgetRequest s],
  _rdcWidth :: Double,
  _rdcSize :: Double
}

radioCfg :: WidgetValue s a -> a -> RadioCfg s e a
radioCfg value option = RadioCfg {
  _rdcValue = value,
  _rdcOption = option,
  _rdcOnChange = [],
  _rdcOnChangeReq = [],
  _rdcWidth = 2,
  _rdcSize = 25
}

radio :: (Eq a) => ALens' s a -> a -> WidgetInstance s e
radio field option = radioInstance where
  config = radioCfg (WidgetLens field) option
  radioInstance = (defaultWidgetInstance "radio" (makeRadio config)) {
    _wiFocusable = True
  }

makeRadio :: (Eq a) => RadioCfg s e a -> Widget s e
makeRadio config = widget where
  widget = createSingle def {
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  handleEvent wenv target evt inst = case evt of
    Click (Point x y) _ -> resultSelected
    KeyAction mod code KeyPressed
      | isSelectKey code -> resultSelected
    _ -> Nothing
    where
      isSelectKey code = isKeyReturn code || isKeySpace code
      reqs = widgetValueSet (_rdcValue config) option
      option = _rdcOption config
      resultSelected = Just $ resultReqs reqs inst

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    sz = _rdcSize config
    size = Size sz sz
    sizeReq = SizeReq size StrictSize StrictSize

  render renderer wenv inst = do
    drawStyledBackground renderer rarea style
    renderRadio renderer config rarea fgColor

    when (value == option) $
      renderMark renderer config rarea fgColor
    where
      model = _weModel wenv
      style = activeStyle wenv inst
      value = widgetValueGet model (_rdcValue config)
      option = _rdcOption config
      rarea = _wiRenderArea inst
      radioL = _rX rarea
      radioT = _rY rarea
      sz = min (_rW rarea) (_rH rarea)
      radioArea = Rect radioL radioT sz sz
      fgColor = activeFgColor wenv inst

renderRadio
  :: (Monad m) => Renderer m -> RadioCfg s e a -> Rect -> Color -> m ()
renderRadio renderer config rect color = action where
  width = _rdcWidth config
  action = drawEllipseBorder renderer rect (Just color) width

renderMark
  :: (Monad m) => Renderer m -> RadioCfg s e a -> Rect -> Color -> m ()
renderMark renderer config rect color = action where
  w = _rdcWidth config * 2
  newRect = subtractFromRect rect w w w w
  action = drawEllipse renderer newRect (Just color)
