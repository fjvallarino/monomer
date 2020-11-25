module Monomer.TestUtil where

import Control.Monad.State
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Sequence (Seq)
import System.IO.Unsafe

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Event
import Monomer.Graphics
import Monomer.Main.Handlers
import Monomer.Main.Types
import Monomer.Main.Util

testW :: Double
testW = 640

testH :: Double
testH = 480

testWindowSize :: Size
testWindowSize = Size testW testH

testWindowRect :: Rect
testWindowRect = Rect 0 0 testW testH

mockTextMetrics :: Font -> FontSize -> TextMetrics
mockTextMetrics font fontSize = TextMetrics {
  _txmAsc = 15,
  _txmDesc = 5,
  _txmLineH = 20
}

mockTextSize :: Font -> FontSize -> Text -> Size
mockTextSize font size text = Size width height where
  width = fromIntegral $ T.length text * 10
  height = 20

mockGlyphsPos :: Font -> FontSize -> Text -> Seq GlyphPos
mockGlyphsPos font fontSize text = glyphs where
  w = 10
  chars = Seq.fromList $ T.unpack text
  mkGlyph idx chr = GlyphPos {
    _glpGlyph = chr,
    _glpXMin = fromIntegral idx * w,
    _glpXMax = (fromIntegral idx + 1) * w,
    _glpW = w
  }
  glyphs = Seq.mapWithIndex mkGlyph chars

mockRenderText :: Point -> Font -> FontSize -> Text -> IO ()
mockRenderText point font size text = return ()

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
  computeTextMetrics = mockTextMetrics,
  computeTextSize = mockTextSize,
  computeGlyphsPos = mockGlyphsPos,
  renderText = mockRenderText,

  -- Image
  addImage = \name action size imgData -> return (),
  updateImage = \name imgData -> return (),
  deleteImage = \name -> return (),
  existsImage = const True,
  renderImage = \name rect alpha -> return ()
}

mockWenv :: s -> WidgetEnv s e
mockWenv model = WidgetEnv {
  _weOS = "Mac OS X",
  _weRenderer = mockRenderer,
  _weTheme = def,
  _weAppWindowSize = testWindowSize,
  _weGlobalKeys = M.empty,
  _weFocusedPath = rootPath,
  _weOverlayPath = Nothing,
  _weCurrentCursor = CursorArrow,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0,
  _weInTopLayer = const True
}

mockWenvEvtUnit :: s -> WidgetEnv s ()
mockWenvEvtUnit model = mockWenv model

instInit :: WidgetEnv s e -> WidgetInstance s e -> WidgetInstance s e
instInit wenv inst = newInst where
  WidgetResult _ _ inst2 = widgetInit (_wiWidget inst) wenv inst
  Size w h = _weAppWindowSize wenv
  vp = Rect 0 0 w h
  newInst = instResize wenv vp inst2

instUpdateSizeReq :: WidgetEnv s e -> WidgetInstance s e -> (SizeReq, SizeReq)
instUpdateSizeReq wenv inst = (sizeReqW,  sizeReqH) where
  WidgetResult _ _ inst2 = widgetInit (_wiWidget inst) wenv inst
  reqInst = widgetUpdateSizeReq (_wiWidget inst2) wenv inst2
  sizeReqW = _wiSizeReqW reqInst
  sizeReqH = _wiSizeReqH reqInst

instResize :: WidgetEnv s e -> Rect -> WidgetInstance s e -> WidgetInstance s e
instResize wenv viewport inst = newInst where
  reqInst = widgetUpdateSizeReq (_wiWidget inst) wenv inst
  newInst = widgetResize (_wiWidget reqInst) wenv viewport viewport reqInst

instHandleEventCtx
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetInstance s e
  -> MonomerContext s
instHandleEventCtx wenv evts inst = ctx where
  ctx = snd $ instHandleEvents wenv evts inst

instHandleEventModel
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetInstance s e
  -> s
instHandleEventModel wenv evts inst = _weModel wenv2 where
  (wenv2, _, _) = fst $ instHandleEvents wenv evts inst

instHandleEventEvts
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetInstance s e
  -> Seq e
instHandleEventEvts wenv evts inst = events where
  (_, events, _) = fst $ instHandleEvents wenv evts inst

instHandleEventRoot
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetInstance s e
  -> WidgetInstance s e
instHandleEventRoot wenv evts inst = newRoot where
  (_, _, newRoot) = fst $ instHandleEvents wenv evts inst

instHandleEvents
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetInstance s e
  -> (HandlerStep s e, MonomerContext s)
instHandleEvents wenv evts inst = unsafePerformIO $ do
  let winSize = testWindowSize
  let vp = Rect 0 0 (_sW winSize) (_sH winSize)
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerContext model undefined winSize useHdpi dpr

  flip runStateT monomerContext $ do
    (wenv2, _, newInst) <- handleWidgetInit wenv inst
    let resizedInst = instResize wenv vp newInst

    handleSystemEvents wenv2 evts resizedInst

roundRectUnits :: Rect -> Rect
roundRectUnits (Rect x y w h) = Rect nx ny nw nh where
  nx = fromIntegral (round x)
  ny = fromIntegral (round y)
  nw = fromIntegral (round w)
  nh = fromIntegral (round h)
