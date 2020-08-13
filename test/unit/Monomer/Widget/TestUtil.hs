module Monomer.Widget.TestUtil where

import Data.Default
import Data.Text (Text)
import Data.Sequence (Seq)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Types
import Monomer.Widget.Types
import Monomer.Widget.Util

testWindowSize :: Size
testWindowSize = Size 640 480

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
  _weScreenSize = testWindowSize,
  _weGlobalKeys = M.empty,
  _weFocusedPath = rootPath,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0
}

initWidget :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
initWidget wenv inst = newInst where
  WidgetResult _ _ inst2 = widgetInit (_wiWidget inst) wenv inst
  Size w h = _weScreenSize wenv
  vp = Rect 0 0 w h
  reqs = widgetUpdateSizeReq (_wiWidget inst2) wenv inst2
  newInst = widgetResize (_wiWidget inst2) wenv vp vp reqs inst2

instanceUpdateSizeReq :: WidgetEnv s e -> WidgetInstance s e -> SizeReq
instanceUpdateSizeReq wenv inst = nodeValue reqs where
  widget = _wiWidget inst
  reqs = widgetUpdateSizeReq widget wenv inst

instanceResize
  :: WidgetEnv s e -> Rect -> WidgetInstance s e -> WidgetInstance s e
instanceResize wenv viewport inst = newInst where
  widget = _wiWidget inst
  reqs = widgetUpdateSizeReq widget wenv inst
  newInst = widgetResize widget wenv viewport viewport reqs inst

instanceGetEvents
  :: WidgetEnv s e
  -> SystemEvent
  -> WidgetInstance s e
  -> Seq e
instanceGetEvents wenv evt inst = events where
  widget = _wiWidget inst
  result = widgetHandleEvent widget wenv rootPath evt inst
  events = maybe Seq.empty _wrEvents result
