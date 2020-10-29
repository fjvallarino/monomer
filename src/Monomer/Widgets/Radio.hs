{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Radio (
  RadioCfg,
  radio,
  radio_,
  radioV,
  radioV_,
  radioD_,
  radioWidth
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data RadioCfg s e a = RadioCfg {
  _rdcWidth :: Maybe Double,
  _rdcOnChange :: [a -> e],
  _rdcOnChangeReq :: [WidgetRequest s]
}

instance Default (RadioCfg s e a) where
  def = RadioCfg {
    _rdcWidth = Nothing,
    _rdcOnChange = [],
    _rdcOnChangeReq = []
  }

instance Semigroup (RadioCfg s e a) where
  (<>) t1 t2 = RadioCfg {
    _rdcWidth = _rdcWidth t2 <|> _rdcWidth t1,
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

radioWidth :: Double -> RadioCfg s e a
radioWidth w = def {
  _rdcWidth = Just w
}

radio :: (Eq a) => ALens' s a -> a -> WidgetInstance s e
radio field option = radio_ field option def

radio_ :: (Eq a) => ALens' s a -> a -> [RadioCfg s e a] -> WidgetInstance s e
radio_ field option configs = radioD_ (WidgetLens field) option configs

radioV :: (Eq a) => a -> (a -> e) -> a -> WidgetInstance s e
radioV value handler option = radioV_ value handler option def

radioV_
  :: (Eq a) => a -> (a -> e) -> a -> [RadioCfg s e a] -> WidgetInstance s e
radioV_ value handler option configs = newInst where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newInst = radioD_ widgetData option newConfigs

radioD_
  :: (Eq a)
  => WidgetData s a
  -> a
  -> [RadioCfg s e a]
  -> WidgetInstance s e
radioD_ widgetData option configs = radioInstance where
  config = mconcat configs
  widget = makeRadio widgetData option config
  radioInstance = (defaultWidgetInstance "radio" widget) {
    _wiFocusable = True
  }

makeRadio :: (Eq a) => WidgetData s a -> a -> RadioCfg s e a -> Widget s e
makeRadio field option config = widget where
  widget = createSingle def {
    singleGetBaseStyle = getBaseStyle,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getBaseStyle wenv inst = Just style where
    style = collectTheme wenv L.radioStyle

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

  getSizeReq wenv inst = req where
    theme = activeTheme wenv inst
    width = fromMaybe (theme ^. L.radioWidth) (_rdcWidth config)
    req = (FixedSize width, FixedSize width)

  render renderer wenv inst = do
    renderRadio renderer radioBW radioArea fgColor

    when (value == option) $
      renderMark renderer radioBW radioArea fgColor
    where
      model = _weModel wenv
      theme = activeTheme wenv inst
      style = activeStyle wenv inst
      value = widgetDataGet model field
      rarea = getContentArea style inst
      radioW = fromMaybe (theme ^. L.radioWidth) (_rdcWidth config)
      radioBW = max 1 (radioW * 0.1)
      radioL = _rX rarea + (_rW rarea - radioW) / 2
      radioT = _rY rarea + (_rH rarea - radioW) / 2
      radioArea = Rect radioL radioT radioW radioW
      fgColor = styleFgColor style

renderRadio :: Renderer -> Double -> Rect -> Color -> IO ()
renderRadio renderer radioBW rect color = action where
  action = drawEllipseBorder renderer rect (Just color) radioBW

renderMark :: Renderer -> Double -> Rect -> Color -> IO ()
renderMark renderer radioBW rect color = action where
  w = radioBW * 2
  newRect = subtractFromRect rect w w w w
  action = drawEllipse renderer newRect (Just color)
