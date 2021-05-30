{-|
Module      : Monomer.TestUtil
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for testing Monomer widgets.
-}
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
  -- Raw overlays
  createRawOverlay  = \overlay -> return (),
  renderRawOverlays = return (),
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
  getImage = const . Just $ ImageDef "test" def BS.empty [],
  addImage = \name size imgData flags -> return (),
  updateImage = \name size imgData -> return (),
  deleteImage = \name -> return (),
  renderImage = \name rect alpha -> return (),
  renderNewImage = \name rect alpha size imgData [] -> return ()
}

mockWenv :: s -> WidgetEnv s e
mockWenv model = WidgetEnv {
  _weOs = "Mac OS X",
  _weRenderer = mockRenderer,
  _weFindByPath = const Nothing,
  _weMainButton = LeftBtn,
  _weTheme = def,
  _weWindowSize = testWindowSize,
  _weGlobalKeys = M.empty,
  _weHoveredPath = Nothing,
  _weFocusedPath = emptyPath,
  _weOverlayPath = Nothing,
  _weDragStatus = Nothing,
  _weMainBtnPress = Nothing,
  _weCursor = Nothing,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0,
  _weInTopLayer = const True,
  _weLayoutDirection = LayoutNone,
  _weViewport = Rect 0 0 testW testH,
  _weOffset = def
}

mockWenvEvtUnit :: s -> WidgetEnv s ()
mockWenvEvtUnit model = mockWenv model

nodeInit :: (Eq s) => WidgetEnv s e -> WidgetNode s e -> WidgetNode s e
nodeInit wenv node = nodeHandleEventRoot wenv [] node

nodeMerge :: WidgetEnv s e -> WidgetNode s e -> WidgetNode s e -> WidgetNode s e
nodeMerge wenv node oldNode = resNode where
  newNode = node
    & L.info . L.path .~ oldNode ^. L.info . L.path
  WidgetResult resNode _ = widgetMerge (newNode^. L.widget) wenv newNode oldNode

nodeGetSizeReq :: WidgetEnv s e -> WidgetNode s e -> (SizeReq, SizeReq)
nodeGetSizeReq wenv node = (sizeReqW,  sizeReqH) where
  WidgetResult node2 _ = widgetInit (node ^. L.widget) wenv node
  sizeReqW = node2 ^. L.info . L.sizeReqW
  sizeReqH = node2 ^. L.info . L.sizeReqH

nodeResize :: WidgetEnv s e -> WidgetNode s e -> Rect -> WidgetNode s e
nodeResize wenv node viewport = result ^. L.node where
  widget = node ^. L.widget
  result = widgetResize widget wenv node viewport

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
  (wenv2, _, _) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEventRoot
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> WidgetNode s e
nodeHandleEventRoot wenv evts node = newRoot where
  (_, newRoot, _) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEventReqs
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq (WidgetRequest s e)
nodeHandleEventReqs wenv evts node = reqs where
  (_, _, reqs) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEventEvts
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> Seq e
nodeHandleEventEvts wenv evts node = eventsFromReqs reqs where
  (_, _, reqs) = fst $ nodeHandleEvents wenv WInit evts node

nodeHandleEvents
  :: (Eq s)
  => WidgetEnv s e
  -> InitWidget
  -> [SystemEvent]
  -> WidgetNode s e
  -> (HandlerStep s e, MonomerCtx s)
nodeHandleEvents wenv init evts node = result where
  steps
    | init == WInit = tail $ nodeHandleEvents_ wenv init [evts] node
    | otherwise = nodeHandleEvents_ wenv init [evts] node
  result = foldl1 stepper steps
  stepper step1 step2 = result where
    ((_, _, reqs1), _) = step1
    ((wenv2, root2, reqs2), ctx2) = step2
    result = ((wenv2, root2, reqs1 <> reqs2), ctx2)

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
    stepA <- if init == WInit
      then do
        handleResourcesInit
        handleWidgetInit wenv pathReadyRoot
      else
        return (wenv, pathReadyRoot, Seq.empty)

    ctxA <- get
    let (wenvA, nodeA, reqsA) = stepA
    let resizeRes = widgetResize (nodeA ^. L.widget) wenv nodeA vp
    step <- handleWidgetResult wenvA True resizeRes
    ctx <- get
    let (wenvr, rootr, reqsr) = step

    (_, _, steps) <- foldM runStep (wenvr, rootr, [(step, ctx)]) evtsG

    return $ if init == WInit
      then (stepA, ctxA) : reverse steps
      else reverse steps
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
      step <- handleSystemEvents wenv root evts
      ctx <- get
      let (wenv2, root2, reqs2) = step

      return (wenv2, root2, (step, ctx) : accum)

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
