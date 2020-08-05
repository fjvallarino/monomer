module Monomer.Widget.TestUtil where

import Data.Text (Text)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Graphics.Types
import Monomer.Widget.Types
import Monomer.Widget.Util

mockTextBounds :: Font -> FontSize -> Text -> Size
mockTextBounds font size text = Size width height where
  width = fromIntegral $ T.length text * 10
  height = 20

mockPlatform :: WidgetPlatform
mockPlatform = WidgetPlatform {
  _wpOS = "Linux",
  _wpGetKeyCode = const Nothing,
  _wpGetTextSize = mockTextBounds
}

mockWenv :: s -> WidgetEnv s e
mockWenv model = WidgetEnv {
  _wePlatform = mockPlatform,
  _weScreenSize = Size 640 480,
  _weGlobalKeys = M.empty,
  _weFocusedPath = rootPath,
  _weModel = model,
  _weInputStatus = undefined, -- :: InputStatus,
  _weTimestamp = 0
}

initWidget :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
initWidget wenv inst = newInst where
  WidgetResult _ _  inst2 = _widgetInit (_wiWidget inst) wenv inst
  Size w h = _weScreenSize wenv
  vp = Rect 0 0 w h
  reqs = _widgetPreferredSize (_wiWidget inst2) wenv inst2
  newInst = _widgetResize (_wiWidget inst2) wenv vp vp reqs inst2

instancePreferredSize :: WidgetEnv s e -> WidgetInstance s e -> SizeReq
instancePreferredSize wenv inst = nodeValue reqs where
  widget = _wiWidget inst
  reqs = _widgetPreferredSize widget wenv inst

instanceResize :: WidgetEnv s e -> Rect -> WidgetInstance s e -> WidgetInstance s e
instanceResize wenv viewport inst = newInst where
  widget = _wiWidget inst
  reqs = _widgetPreferredSize widget wenv inst
  newInst = _widgetResize widget wenv viewport viewport reqs inst
