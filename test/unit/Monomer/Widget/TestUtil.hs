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
import Monomer.Graphics.Renderer
import Monomer.Graphics.Types
import Monomer.Widget.Types
import Monomer.Widget.Util

testWindowSize :: Size
testWindowSize = Size 640 480

mockTextBounds :: Font -> FontSize -> Text -> Size
mockTextBounds font size text = Size width height where
  width = fromIntegral $ T.length text * 10
  height = 20

mockRenderer :: Renderer
mockRenderer = Renderer {
  beginFrame = \w h -> return (),
  endFrame = return (),
  -- Path
  beginPath = return (),
  closePath = return (),
  -- Context management
  saveContext = return (),
  restoreContext = return (),
  -- Overlays
  createOverlay  = \overlay -> return (),
  renderOverlays = return (),
  -- Scissor operations
  setScissor = \rect -> return (),
  resetScissor = return (),
  -- Strokes
  stroke = return (),
  setStrokeColor = \color -> return (),
  setStrokeWidth = \width -> return (),
  -- Fill
  fill = return (),
  setFillColor = \color -> return (),
  setFillLinearGradient = \p1 p2 c1 c2 -> return (),
  -- Drawing
  moveTo = \point -> return (),
  renderLine = \p1 p2 -> return (),
  renderLineTo = \point -> return (),
  renderRect = \rect -> return (),
  renderArc = \center radius angleStart angleEnd winding -> return (),
  renderQuadTo = \p1 p2 -> return (),
  renderEllipse = \rect -> return (),
  -- Text
  computeTextSize = mockTextBounds,
  renderText = \rect font size align text -> return def,
  -- Image
  addImage = \name w h replace imgData -> return (),
  updateImage = \name imgData -> return (),
  deleteImage = \name -> return (),
  renderImage = \rect name -> return ()
}

mockPlatform :: WidgetPlatform
mockPlatform = WidgetPlatform {
  _wpOS = "Linux",
  _wpGetKeyCode = const Nothing,
  _wpComputeTextSize = mockTextBounds
}

mockWenv :: s -> WidgetEnv s e
mockWenv model = WidgetEnv {
  _wePlatform = mockPlatform,
  _weRenderer = mockRenderer,
  _weTheme = def,
  _weAppWindowSize = testWindowSize,
  _weGlobalKeys = M.empty,
  _weFocusedPath = rootPath,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0
}

initWidget :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
initWidget wenv inst = newInst where
  WidgetResult _ _ inst2 = widgetInit (_wiWidget inst) wenv inst
  Size w h = _weAppWindowSize wenv
  vp = Rect 0 0 w h
  reqInst = widgetUpdateSizeReq (_wiWidget inst2) wenv inst2
  newInst = widgetResize (_wiWidget inst2) wenv vp vp reqInst

instanceUpdateSizeReq :: WidgetEnv s e -> WidgetInstance s e -> SizeReq
instanceUpdateSizeReq wenv inst = _wiSizeReq reqInst where
  widget = _wiWidget inst
  reqInst = widgetUpdateSizeReq widget wenv inst

instanceResize
  :: WidgetEnv s e -> Rect -> WidgetInstance s e -> WidgetInstance s e
instanceResize wenv viewport inst = newInst where
  widget = _wiWidget inst
  reqInst = widgetUpdateSizeReq widget wenv inst
  newInst = widgetResize widget wenv viewport viewport reqInst

instanceGetEvents
  :: WidgetEnv s e
  -> SystemEvent
  -> WidgetInstance s e
  -> Seq e
instanceGetEvents wenv evt inst = events where
  widget = _wiWidget inst
  result = widgetHandleEvent widget wenv rootPath evt inst
  events = maybe Seq.empty _wrEvents result
