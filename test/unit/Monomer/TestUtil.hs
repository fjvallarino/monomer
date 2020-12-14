module Monomer.TestUtil where

import Control.Lens ((&), (^.), (.~))
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
import Monomer.Widgets.Util.Widget

import qualified Monomer.Lens as L

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
  _weWindowSize = testWindowSize,
  _weGlobalKeys = M.empty,
  _weFocusedPath = rootPath,
  _weOverlayPath = Nothing,
  _wePressedPath = Nothing,
  _weCurrentCursor = CursorArrow,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0,
  _weInTopLayer = const True
}

mockWenvEvtUnit :: s -> WidgetEnv s ()
mockWenvEvtUnit model = mockWenv model

nodeInit :: WidgetEnv s e -> WidgetNode s e -> WidgetNode s e
nodeInit wenv node = newNode where
  WidgetResult node2 _ _ = widgetInit (node ^. L.widget) wenv node
  Size w h = _weWindowSize wenv
  vp = Rect 0 0 w h
  newNode = nodeResize wenv vp node2

nodeUpdateSizeReq :: WidgetEnv s e -> WidgetNode s e -> (SizeReq, SizeReq)
nodeUpdateSizeReq wenv node = (sizeReqW,  sizeReqH) where
  WidgetResult node2 _ _ = widgetInit (node ^. L.widget) wenv node
  reqNode = widgetUpdateSizeReq (node2 ^. L.widget) wenv node2
  sizeReqW = reqNode ^. L.sizeReqW
  sizeReqH = reqNode ^. L.sizeReqH

nodeResize :: WidgetEnv s e -> Rect -> WidgetNode s e -> WidgetNode s e
nodeResize wenv viewport node = newNode where
  newNode = resizeWidget wenv viewport viewport node

nodeHandleEventCtx
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> MonomerContext s
nodeHandleEventCtx wenv evts node = ctx where
  ctx = snd $ nodeHandleEvents wenv evts node

nodeHandleEventCtxModel
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> s
nodeHandleEventCtxModel wenv evts node = ctx where
  ctx = _mcMainModel (nodeHandleEventCtx wenv evts node)

nodeHandleEventModel
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> s
nodeHandleEventModel wenv evts node = _weModel wenv2 where
  (wenv2, _, _) = fst $ nodeHandleEvents wenv evts node

nodeHandleEventEvts
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq e
nodeHandleEventEvts wenv evts node = events where
  (_, events, _) = fst $ nodeHandleEvents wenv evts node

nodeHandleEventRoot
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> WidgetNode s e
nodeHandleEventRoot wenv evts node = newRoot where
  (_, _, newRoot) = fst $ nodeHandleEvents wenv evts node

nodeHandleEvents
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerContext s)
nodeHandleEvents wenv evts node = unsafePerformIO $ do
  let winSize = testWindowSize
  let vp = Rect 0 0 (_sW winSize) (_sH winSize)
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerContext model undefined winSize useHdpi dpr

  flip runStateT monomerContext $ do
    (wenv2, _, newNode) <- handleWidgetInit wenv node
    let resizedNode = nodeResize wenv vp newNode

    handleSystemEvents wenv2 evts resizedNode

nodeHandleEventModelNoInit
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> s
nodeHandleEventModelNoInit wenv evts node = _weModel wenv2 where
  (wenv2, _, _) = fst $ nodeHandleEventsNoInit wenv evts node

nodeHandleEventsNoInit
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerContext s)
nodeHandleEventsNoInit wenv evts node = unsafePerformIO $ do
  let winSize = testWindowSize
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerContext model undefined winSize useHdpi dpr

  flip runStateT monomerContext $
    handleSystemEvents wenv evts node

roundRectUnits :: Rect -> Rect
roundRectUnits (Rect x y w h) = Rect nx ny nw nh where
  nx = fromIntegral (round x)
  ny = fromIntegral (round y)
  nw = fromIntegral (round w)
  nh = fromIntegral (round h)
