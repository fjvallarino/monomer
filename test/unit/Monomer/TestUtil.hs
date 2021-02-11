{-# LANGUAGE FlexibleContexts #-}

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

data InitWidget
  = WInit
  | WNoInit
  deriving (Eq, Show)

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

mockTextSize :: Maybe Double -> Font -> FontSize -> Text -> Size
mockTextSize mw font (FontSize fs) text = Size width height where
  w = fromMaybe fs mw
  width = fromIntegral (T.length text) * w
  height = 20

mockGlyphsPos :: Maybe Double -> Font -> FontSize -> Text -> Seq GlyphPos
mockGlyphsPos mw font (FontSize fs) text = glyphs where
  w = fromMaybe fs mw
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
  intersectScissor = \rect -> return (),
  -- Translation
  setTranslation = \point -> return (),
  -- Scale
  setScale = \point -> return (),
  -- Rotation
  setRotation = \angle -> return (),
  -- Global Alpha
  setGlobalAlpha = \alpha -> return (),
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
  computeTextSize = mockTextSize (Just 10),
  computeGlyphsPos = mockGlyphsPos (Just 10),
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
  _weDragStatus = Nothing,
  _weMainBtnPress = Nothing,
  _weCurrentCursor = CursorArrow,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0,
  _weInTopLayer = const True,
  _weViewport = Rect 0 0 testW testH,
  _weOffset = def
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
  result = widgetResize widget wenv viewport node

nodeHandleEventCtx
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> MonomerCtx s
nodeHandleEventCtx wenv evts node = ctx where
  ctx = snd $ nodeHandleEvents wenv WInit evts node

nodeHandleEventModel
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> s
nodeHandleEventModel wenv evts node = _weModel wenv2 where
  (wenv2, _, _, _) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEventRoot
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> WidgetNode s e
nodeHandleEventRoot wenv evts node = newRoot where
  (_, newRoot, _, _) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEventReqs
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq (WidgetRequest s)
nodeHandleEventReqs wenv evts node = reqs where
  (_, _, reqs, _) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEventEvts
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq e
nodeHandleEventEvts wenv evts node = events where
  (_, _, _, events) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEvents
  :: (Eq s)
  => WidgetEnv s e
  -> InitWidget
  -> [SystemEvent]
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerCtx s)
nodeHandleEvents wenv init evts node = result where
  steps = nodeHandleEvents_ wenv init [evts] node
  result = foldl1 stepper steps
  stepper step1 step2 = result where
    ((_, _, reqs1, evts1), _) = step1
    ((wenv2, root2, reqs2, evts2), ctx2) = step2
    result = ((wenv2, root2, reqs1 <> reqs2, evts1 <> evts2), ctx2)

nodeHandleEvents_
  :: (Eq s)
  => WidgetEnv s e
  -> InitWidget
  -> [[SystemEvent]]
  -> WidgetNode s e
  -> [(HandlerStep s e, MonomerCtx s)]
nodeHandleEvents_ wenv init evtsG node = unsafePerformIO $ do
  -- Do NOT test code involving SDL Window functions
  fmap fst $ flip runStateT monomerContext $ do
    (wenv2, newNode, _, _) <- if init == WInit
      then do
        handleResourcesInit
        handleWidgetInit wenv pathReadyRoot
      else
        return (wenv, pathReadyRoot, Seq.empty, Seq.empty)

    let resizeRes = widgetResize (newNode ^. L.widget) wenv vp newNode
    step <- handleWidgetResult wenv2 True resizeRes
    ctx <- get
    let (wenvr, rootr, reqsr, evtsr) = step

    (_, _, steps) <- foldM runStep (wenvr, rootr, [(step, ctx)]) evtsG

    return (reverse steps)
  where
    winSize = _weWindowSize wenv
    vp = Rect 0 0 (_sW winSize) (_sH winSize)
    useHdpi = True
    dpr = 1
    model = _weModel wenv
    monomerContext = initMonomerCtx model undefined winSize useHdpi dpr
    pathReadyRoot = node
      & L.info . L.path .~ rootPath
      & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) rootPath
    runStep (wenv, root, accum) evts = do
      step <- handleSystemEvents wenv evts root
      ctx <- get
      let (wenv2, root2, reqs2, evts2) = step

      return (wenv2, root2, (step, ctx) : accum)

nodeHandleRestore
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetInstanceNode
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerCtx s)
nodeHandleRestore wenv evts inst node = unsafePerformIO $ do
  let winSize = _weWindowSize wenv
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerCtx model undefined winSize useHdpi dpr
  let pathReadyRoot = node
        & L.info . L.path .~ rootPath
        & L.info . L.widgetId .~ WidgetId (wenv ^. L.timestamp) rootPath

  flip runStateT monomerContext $ do
    (wenv2, root2, reqs2, evts2) <- handleWidgetRestore wenv inst pathReadyRoot
    (wenv3, root3, reqs3, evts3) <- handleSystemEvents wenv2 evts root2
    return (wenv3, root3, reqs2 <> reqs3, evts2 <> evts3)

nodeHandleResult
  :: (Eq s)
  => WidgetEnv s e
  -> WidgetResult s e
  -> (HandlerStep s e, MonomerCtx s)
nodeHandleResult wenv result = unsafePerformIO $ do
  let winSize = _weWindowSize wenv
  let useHdpi = True
  let dpr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerContext = initMonomerCtx model undefined winSize useHdpi dpr

  flip runStateT monomerContext $ do
    handleWidgetResult wenv True result

roundRectUnits :: Rect -> Rect
roundRectUnits (Rect x y w h) = Rect nx ny nw nh where
  nx = fromIntegral (round x)
  ny = fromIntegral (round y)
  nw = fromIntegral (round w)
  nh = fromIntegral (round h)
