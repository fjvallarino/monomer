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

import Control.Concurrent (newMVar)
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Lens ((&), (^.), (.~), (.=))
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
  _txmLineH = 20,
  _txmLowerX = 10
}

mockTextSize :: Maybe Double -> Font -> FontSize -> FontSpace -> Text -> Size
mockTextSize mw font (FontSize fs) spaceH text = Size width height where
  w = fromMaybe fs mw + unFontSpace spaceH
  width = fromIntegral (T.length text) * w
  height = 20

mockGlyphsPos
  :: Maybe Double -> Font -> FontSize -> FontSpace -> Text -> Seq GlyphPos
mockGlyphsPos mw font (FontSize fs) spaceH text = glyphs where
  w = fromMaybe fs mw + unFontSpace spaceH
  chars = Seq.fromList $ T.unpack text
  mkGlyph idx chr = GlyphPos {
    _glpGlyph = chr,
    _glpXMin = fromIntegral idx * w,
    _glpXMax = (fromIntegral idx + 1) * w,
    _glpYMin = 0,
    _glpYMax = 10,
    _glpW = w,
    _glpH = 10
  }
  glyphs = Seq.mapWithIndex mkGlyph chars

mockRenderText :: Point -> Font -> FontSize -> FontSpace -> Text -> IO ()
mockRenderText point font size spaceH text = return ()

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
  createOverlay = \overlay -> return (),
  renderOverlays = return (),
  -- Raw tasks
  createRawTask = \task -> return (),
  renderRawTasks = return (),
  -- Raw overlays
  createRawOverlay = \overlay -> return (),
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
  -- Path Winding
  setPathWinding = \winding -> return (),
  -- Strokes
  stroke = return (),
  setStrokeColor = \color -> return (),
  setStrokeWidth = \width -> return (),
  setStrokeLinearGradient = \p1 p2 c1 c2 -> return (),
  setStrokeRadialGradient = \p1 a1 a2 c1 c2 -> return (),
  setStrokeImagePattern = \n1 p1 s1 w1 h1 -> return (),
  -- Fill
  fill = return (),
  setFillColor = \color -> return (),
  setFillLinearGradient = \p1 p2 c1 c2 -> return (),
  setFillRadialGradient = \p1 a1 a2 c1 c2 -> return (),
  setFillImagePattern = \n1 p1 s1 w1 h1 -> return (),
  -- Drawing
  moveTo = \point -> return (),
  renderLine = \p1 p2 -> return (),
  renderLineTo = \point -> return (),
  renderRect = \rect -> return (),
  renderRoundedRect = \rect r1 r2 r3 r4 -> return (),
  renderArc = \center radius angleStart angleEnd winding -> return (),
  renderQuadTo = \p1 p2 -> return (),
  renderEllipse = \rect -> return (),
  -- Text
  renderText = mockRenderText,

  -- Image
  getImage = const . return $ Just $ ImageDef "test" def BS.empty [],
  addImage = \name size imgData flags -> return (),
  updateImage = \name size imgData -> return (),
  deleteImage = \name -> return ()
}

mockFontManager :: FontManager
mockFontManager = FontManager {
  computeTextMetrics = mockTextMetrics,
  computeTextSize = mockTextSize (Just 10),
  computeGlyphsPos = mockGlyphsPos (Just 10)
}

mockWenv :: s -> WidgetEnv s e
mockWenv model = WidgetEnv {
  _weOs = "Mac OS X",
  _weFontManager = mockFontManager,
  _weFindByPath = const Nothing,
  _weMainButton = BtnLeft,
  _weContextButton = BtnRight,
  _weTheme = def,
  _weWindowSize = testWindowSize,
  _weWidgetShared = unsafePerformIO (newMVar M.empty),
  _weWidgetKeyMap = M.empty,
  _weHoveredPath = Nothing,
  _weFocusedPath = emptyPath,
  _weOverlayPath = Nothing,
  _weDragStatus = Nothing,
  _weMainBtnPress = Nothing,
  _weCursor = Nothing,
  _weModel = model,
  _weInputStatus = def,
  _weTimestamp = 0,
  _weThemeChanged = False,
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
  resizeCheck = const True
  widget = node ^. L.widget
  result = widgetResize widget wenv node viewport resizeCheck

nodeHandleEventCtx
  :: (Eq s)
  => WidgetEnv s e
  -> [SystemEvent]
  -> WidgetNode s e
  -> MonomerCtx s e
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
  -> (HandlerStep s e, MonomerCtx s e)
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
  -> [(HandlerStep s e, MonomerCtx s e)]
nodeHandleEvents_ wenv init evtsG node = unsafePerformIO $ do
  -- Do NOT test code involving SDL Window functions
  channel <- liftIO newTChanIO

  fmap fst $ flip runStateT (monomerCtx channel) $ do
    L.inputStatus .= wenv ^. L.inputStatus
    handleResourcesInit

    stepA <- if init == WInit
      then do
        handleWidgetInit wenv pathReadyRoot
      else
        return (wenv, pathReadyRoot, Seq.empty)

    ctxA <- get
    let (wenvA, nodeA, reqsA) = stepA
    let resizeCheck = const True
    let resizeRes = widgetResize (nodeA ^. L.widget) wenv nodeA vp resizeCheck
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
    dpr = 1
    epr = 1
    model = _weModel wenv
    monomerCtx channel = initMonomerCtx undefined channel winSize dpr epr model
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
  -> (HandlerStep s e, MonomerCtx s e)
nodeHandleResult wenv result = unsafePerformIO $ do
  channel <- liftIO newTChanIO

  let winSize = _weWindowSize wenv
  let dpr = 1
  let epr = 1
  let model = _weModel wenv
  -- Do NOT test code involving SDL Window functions
  let monomerCtx = initMonomerCtx undefined channel winSize dpr epr model

  flip runStateT monomerCtx $ do
    L.inputStatus .= wenv ^. L.inputStatus
    handleResourcesInit

    handleWidgetResult wenv True result

roundRectUnits :: Rect -> Rect
roundRectUnits (Rect x y w h) = Rect nx ny nw nh where
  nx = fromIntegral (round x)
  ny = fromIntegral (round y)
  nw = fromIntegral (round w)
  nh = fromIntegral (round h)
