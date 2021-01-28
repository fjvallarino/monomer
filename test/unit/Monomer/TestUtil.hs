module Monomer.TestUtil where

import Control.Lens ((&), (^.), (.~))
import Control.Monad.State
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Sequence (Seq)
import System.IO.Unsafe

import qualified Data.ByteString as BS
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
  getImage = const . Just $ ImageDef "test" def BS.empty,
  addImage = \name size imgData -> return (),
  updateImage = \name size imgData -> return (),
  deleteImage = \name -> return (),
  renderImage = \name rect alpha -> return (),
  renderNewImage = \name size imgData rect alpha -> return ()
}

mockWenv :: s -> WidgetEnv s e
mockWenv model = WidgetEnv {
  _weOS = "Mac OS X",
  _weRenderer = mockRenderer,
  _weMainButton = LeftBtn,
  _weTheme = def,
  _weWindowSize = testWindowSize,
  _weGlobalKeys = M.empty,
  _weFocusedPath = emptyPath,
  _weOverlayPath = Nothing,
  _weMainBtnPress = Nothing,
  _weCurrentCursor = CursorArrow,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0,
  _weInTopLayer = const True
}

mockWenvEvtUnit :: s -> WidgetEnv s ()
mockWenvEvtUnit model = mockWenv model

nodeInit :: (Eq s) => WidgetEnv s e -> WidgetNode s e -> WidgetNode s e
nodeInit wenv node = nodeHandleEventRoot wenv [] node

nodeMerge :: WidgetEnv s e -> WidgetNode s e -> WidgetNode s e -> WidgetNode s e
nodeMerge wenv oldNode node = newNode where
  WidgetResult newNode _ _ = widgetMerge (node ^. L.widget) wenv oldNode $ node
    & L.info . L.path .~ oldNode ^. L.info . L.path

nodeGetSizeReq :: WidgetEnv s e -> WidgetNode s e -> (SizeReq, SizeReq)
nodeGetSizeReq wenv node = (sizeReqW,  sizeReqH) where
  WidgetResult node2 _ _ = widgetInit (node ^. L.widget) wenv node
  sizeReqW = node2 ^. L.info . L.sizeReqW
  sizeReqH = node2 ^. L.info . L.sizeReqH

nodeResize :: WidgetEnv s e -> Rect -> WidgetNode s e -> WidgetNode s e
nodeResize wenv viewport node = result ^. L.node where
  widget = node ^. L.widget
  result = widgetResize widget wenv viewport viewport node

nodeHandleEventCtx
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> MonomerCtx s
nodeHandleEventCtx wenv evts node = ctx where
  ctx = snd $ nodeHandleEvents wenv evts node

nodeHandleEventModel
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> s
nodeHandleEventModel wenv evts node = _weModel wenv2 where
  (wenv2, _, _, _) = fst $ nodeHandleEvents wenv evts node

nodeHandleEventRoot
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> WidgetNode s e
nodeHandleEventRoot wenv evts node = newRoot where
  (_, newRoot, _, _) = fst $ nodeHandleEvents wenv evts node

nodeHandleEventReqs
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq (WidgetRequest s)
nodeHandleEventReqs wenv evts node = reqs where
  (_, _, reqs, _) = fst $ nodeHandleEvents wenv evts node

nodeHandleEventEvts
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq e
nodeHandleEventEvts wenv evts node = events where
  (_, _, _, events) = fst $ nodeHandleEvents wenv evts node

nodeHandleEvents
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerCtx s)
nodeHandleEvents wenv evts node = unsafePerformIO $ do
  let winSize = _weWindowSize wenv
  let vp = Rect 0 0 (_sW winSize) (_sH winSize)
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerCtx model undefined winSize useHdpi dpr
  let pathReadyRoot = node
        & L.info . L.path .~ Seq.singleton 0
        & L.info . L.widgetId .~ WidgetId (wenv ^.L.timestamp) (Seq.singleton 0)

  flip runStateT monomerContext $ do
    handleResourcesInit
    (wenv2, newNode, _, _) <- handleWidgetInit wenv pathReadyRoot

    let resizeRes = resizeRoot wenv2 winSize newNode
    (wenv3, sizedNode, reqs1, evts1) <- handleWidgetResult wenv2 True resizeRes
    (wenv4, newRoot, reqs2, evts2) <- handleSystemEvents wenv2 evts sizedNode

    return (wenv4, newRoot, reqs1 <> reqs2, evts1 <> evts2)

nodeHandleEventModelNoInit
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> s
nodeHandleEventModelNoInit wenv evts node = _weModel wenv2 where
  (wenv2, _, _, _) = fst $ nodeHandleEventsNoInit wenv evts node

nodeHandleEventRootNoInit
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> WidgetNode s e
nodeHandleEventRootNoInit wenv evts node = newRoot where
  (_, newRoot, _, _) = fst $ nodeHandleEventsNoInit wenv evts node

nodeHandleEventReqsNoInit
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq (WidgetRequest s)
nodeHandleEventReqsNoInit wenv evts node = newReqs where
  (_, _, newReqs, _) = fst $ nodeHandleEventsNoInit wenv evts node

nodeHandleEventsNoInit
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerCtx s)
nodeHandleEventsNoInit wenv evts node = unsafePerformIO $ do
  let winSize = _weWindowSize wenv
  let vp = Rect 0 0 (_sW winSize) (_sH winSize)
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerCtx model undefined winSize useHdpi dpr
  let pathReadyRoot = node
        & L.info . L.path .~ Seq.singleton 0
        & L.info . L.widgetId .~ WidgetId (wenv ^.L.timestamp) (Seq.singleton 0)

  flip runStateT monomerContext $ do
    let resizedNode = nodeResize wenv vp pathReadyRoot

    handleSystemEvents wenv evts resizedNode

nodeHandleRestore
  :: (Eq s)
  => WidgetEnv s e
  -> WidgetInstanceNode
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerCtx s)
nodeHandleRestore wenv inst node = unsafePerformIO $ do
  let winSize = _weWindowSize wenv
  let vp = Rect 0 0 (_sW winSize) (_sH winSize)
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerCtx model undefined winSize useHdpi dpr
  let pathReadyRoot = node
        & L.info . L.path .~ Seq.singleton 0
        & L.info . L.widgetId .~ WidgetId (wenv ^.L.timestamp) (Seq.singleton 0)

  flip runStateT monomerContext $
    handleWidgetRestore wenv inst pathReadyRoot

roundRectUnits :: Rect -> Rect
roundRectUnits (Rect x y w h) = Rect nx ny nw nh where
  nx = fromIntegral (round x)
  ny = fromIntegral (round y)
  nw = fromIntegral (round w)
  nh = fromIntegral (round h)
