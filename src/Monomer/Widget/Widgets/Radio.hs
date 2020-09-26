{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widget.Widgets.Radio (
  RadioCfg,
  radio,
  radio_,
  radioV,
  radioV_,
  radioD_
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

data RadioCfg s e a = RadioCfg {
  _rdcOnChange :: [a -> e],
  _rdcOnChangeReq :: [WidgetRequest s]
}

instance Default (RadioCfg s e a) where
  def = RadioCfg {
    _rdcOnChange = [],
    _rdcOnChangeReq = []
  }

instance Semigroup (RadioCfg s e a) where
  (<>) t1 t2 = RadioCfg {
    _rdcOnChange = _rdcOnChange t1 <> _rdcOnChange t2,
    _rdcOnChangeReq = _rdcOnChangeReq t1 <> _rdcOnChangeReq t2
  }

instance Monoid (RadioCfg s e a) where
  mempty = def

instance OnChange (RadioCfg s e a) a e where
  onChange fn = def {
    _rdcOnChange = [fn]
  }

instance OnChangeReq (RadioCfg s e a) s where
  onChangeReq req = def {
    _rdcOnChangeReq = [req]
  }

radioWidth :: Double
radioWidth = 25

radioBorderW :: Double
radioBorderW = 2

radio :: (Eq a) => ALens' s a -> a -> WidgetInstance s e
radio field option = radio_ field option def

radio_ :: (Eq a) => ALens' s a -> a -> RadioCfg s e a -> WidgetInstance s e
radio_ field option config = radioD_ (WidgetLens field) option config

radioV :: (Eq a) => a -> (a -> e) -> a -> WidgetInstance s e
radioV value handler option = radioV_ value handler option def

radioV_ :: (Eq a) => a -> (a -> e) -> a -> RadioCfg s e a -> WidgetInstance s e
radioV_ value handler option config = radioD_ widgetData option newConfig where
  widgetData = WidgetValue value
  newConfig = config <> onChange handler

radioD_
  :: (Eq a)
  => WidgetData s a
  -> a
  -> RadioCfg s e a
  -> WidgetInstance s e
radioD_ widgetData option config = radioInstance where
  widget = makeRadio widgetData option config
  radioInstance = (defaultWidgetInstance "radio" widget) {
    _wiFocusable = True
  }

makeRadio :: (Eq a) => WidgetData s a -> a -> RadioCfg s e a -> Widget s e
makeRadio field option config = widget where
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
      events = fmap ($ option) (_rdcOnChange config)
      setValueReq = widgetDataSet field option
      setFocusReq = SetFocus $ _wiPath inst
      reqs = setValueReq ++ _rdcOnChangeReq config
      clickReqs = setFocusReq : reqs

  getSizeReq wenv inst = sizeReq where
    style = activeStyle wenv inst
    sz = radioWidth
    size = Size sz sz
    sizeReq = SizeReq size StrictSize StrictSize

  render renderer wenv inst = do
    renderRadio renderer config rarea fgColor

    when (value == option) $
      renderMark renderer config rarea fgColor
    where
      model = _weModel wenv
      style = activeStyle wenv inst
      value = widgetDataGet model field
      rarea = removeOuterBounds style $ _wiRenderArea inst
      radioL = _rX rarea
      radioT = _rY rarea
      sz = min (_rW rarea) (_rH rarea)
      radioArea = Rect radioL radioT sz sz
      fgColor = instanceFgColor wenv inst

renderRadio :: Renderer -> RadioCfg s e a -> Rect -> Color -> IO ()
renderRadio renderer config rect color = action where
  width = radioBorderW
  action = drawEllipseBorder renderer rect (Just color) width

renderMark :: Renderer -> RadioCfg s e a -> Rect -> Color -> IO ()
renderMark renderer config rect color = action where
  w = radioBorderW
  newRect = subtractFromRect rect w w w w
  action = drawEllipse renderer newRect (Just color)
