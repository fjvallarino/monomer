{-|
Module      : Monomer.Main.Platform
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for SDL platform related operations.
-}
{-# LANGUAGE Strict #-}

module Monomer.Main.Platform (
  defaultWindowSize,
  initSDLWindow,
  detroySDLWindow,
  getCurrentMousePos,
  getDrawableSize,
  getWindowSize,
  getViewportSize,
  getPlatform,
  getDisplayDPI
) where

import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.Extra (whenJust)
import Control.Monad.State
import Data.Maybe
import Data.Text (Text)
import Foreign (alloca, peek)
import Foreign.C (peekCString, withCString)
import Foreign.C.Types
import SDL (($=))

import qualified Data.Text as T
import qualified Foreign.C.String as STR
import qualified SDL
import qualified SDL.Input.Mouse as Mouse
import qualified SDL.Raw as Raw
import qualified SDL.Raw.Error as SRE
import qualified SDL.Internal.Types as SIT
import qualified SDL.Video.Renderer as SVR

import Monomer.Common
import Monomer.Core.StyleTypes
import Monomer.Main.Types
import Monomer.Event.Types
import Monomer.Widgets.Composite

foreign import ccall unsafe "initGlew" glewInit :: IO CInt
foreign import ccall unsafe "initDpiAwareness" initDpiAwareness :: IO CInt

-- | Default window size if not is specified.
defaultWindowSize :: (Int, Int)
defaultWindowSize = (800, 600)

-- | Creates and initializes a window using the provided configuration.
initSDLWindow :: AppConfig e -> IO (SDL.Window, Double, Double, SDL.GLContext)
initSDLWindow config = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  setDisableCompositorHint compositingFlag

  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  platform <- getPlatform
  initDpiAwareness

  baseFactor <- case platform of
    "Windows" -> getWindowsFactor
    "Linux" -> getLinuxFactor
    _ -> return 1 -- macOS

  let factor
        | disableAutoScale = 1
        | otherwise = baseFactor
  let (winW, winH) = (factor * fromIntegral baseW, factor * fromIntegral baseH)

  window <-
    SDL.createWindow
      "Monomer application"
      SDL.defaultWindow {
        SDL.windowInitialSize = SDL.V2 (round winW) (round winH),
        SDL.windowHighDPI = True,
        SDL.windowResizable = windowResizable,
        SDL.windowBorder = windowBorder,
        SDL.windowGraphicsContext = SDL.OpenGLContext customOpenGL
      }

  -- Get device pixel rate
  Size dw _ <- getDrawableSize window
  Size ww _ <- getWindowSize window
  let scaleFactor = factor * userScaleFactor
  let contentRatio = dw / ww
  let (dpr, epr)
        | platform `elem` ["Windows", "Linux"] = (scaleFactor, 1 / scaleFactor)
        | otherwise = (scaleFactor * contentRatio, 1 / scaleFactor) -- macOS

  setWindowIcon window config

  whenJust (_apcWindowTitle config) $ \title ->
    SDL.windowTitle window $= title

  when windowFullscreen $
    SDL.setWindowMode window SDL.FullscreenDesktop

  when windowMaximized $
    SDL.setWindowMode window SDL.Maximized

  err <- SRE.getError
  err <- STR.peekCString err
  putStrLn err

  ctxRender <- SDL.glCreateContext window

  when (platform == "Windows") $
    void $ SDL.glCreateContext window

  _ <- glewInit

  return (window, dpr, epr, ctxRender)
  where
    customOpenGL = SDL.OpenGLConfig {
      SDL.glColorPrecision = SDL.V4 8 8 8 0,
      SDL.glDepthPrecision = 24,
      SDL.glStencilPrecision = 8,
      --SDL.glProfile = SDL.Core SDL.Debug 3 2,
      SDL.glProfile = SDL.Core SDL.Normal 3 2,
      SDL.glMultisampleSamples = 1
    }
    compositingFlag = fromMaybe False (_apcDisableCompositing config)
    userScaleFactor = fromMaybe 1 (_apcScaleFactor config)
    disableAutoScale = _apcDisableAutoScale config == Just True
    (baseW, baseH) = case _apcWindowState config of
      Just (MainWindowNormal size) -> size
      _ -> defaultWindowSize
    windowResizable = fromMaybe True (_apcWindowResizable config)
    windowBorder = fromMaybe True (_apcWindowBorder config)
    windowFullscreen = case _apcWindowState config of
      Just MainWindowFullScreen -> True
      _ -> False
    windowMaximized = case _apcWindowState config of
      Just MainWindowMaximized -> True
      _ -> False

setWindowIcon :: SDL.Window -> AppConfig e -> IO ()
setWindowIcon (SIT.Window winPtr) config =
  forM_ (_apcWindowIcon config) $ \iconPath -> do
    iconSurface <- SVR.loadBMP (T.unpack iconPath)
    let SVR.Surface iconSurfacePtr _ = iconSurface
    finally
      -- Note: this can use the high-level setWindowIcon once it is available (https://github.com/haskell-game/sdl2/pull/243)
      (Raw.setWindowIcon winPtr iconSurfacePtr)
      (SVR.freeSurface iconSurface)

-- | Destroys the provided window, shutdowns the video subsystem and SDL.
detroySDLWindow :: SDL.Window -> IO ()
detroySDLWindow window = do
  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  Raw.quitSubSystem Raw.SDL_INIT_VIDEO
  SDL.quit

-- | Returns the current mouse position.
getCurrentMousePos :: Double -> IO Point
getCurrentMousePos epr = do
  SDL.P (SDL.V2 x y) <- Mouse.getAbsoluteMouseLocation
  return $ Point (epr * fromIntegral x) (epr * fromIntegral y)

-- | Returns the drawable size of the provided window. May differ from window
--   size if HDPI is enabled.
getDrawableSize :: SDL.Window -> IO Size
getDrawableSize window = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  return $ Size (fromIntegral fbWidth) (fromIntegral fbHeight)

-- | Returns the size of the provided window.
getWindowSize :: SDL.Window -> IO Size
getWindowSize window = do
  SDL.V2 rw rh <- SDL.get (SDL.windowSize window)

  return $ Size (fromIntegral rw) (fromIntegral rh)

{-|
Returns the viewport size. This is the size of the viewport the application will
render to and, depending on the platform, may match window size or not. For
example, on Windows and Linux Wayland this size may be smaller than the window
size because of dpr scaling.
-}
getViewportSize :: SDL.Window -> Double -> IO Size
getViewportSize window dpr = do
  SDL.V2 fw fh <- SDL.glGetDrawableSize window

  return $ Size (fromIntegral fw / dpr) (fromIntegral fh / dpr)

-- | Returns the name of the host OS.
getPlatform :: IO Text
getPlatform = do
  platform <- peekCString =<< Raw.getPlatform

  return $ T.pack platform

-- | Returns the diagonal, horizontal and vertical DPI of the main display.
getDisplayDPI :: IO (Double, Double, Double)
getDisplayDPI =
  alloca $ \pddpi ->
    alloca $ \phdpi ->
      alloca $ \pvdpi -> do
        Raw.getDisplayDPI 0 pddpi phdpi pvdpi
        ddpi <- peek pddpi
        hdpi <- peek phdpi
        vdpi <- peek pvdpi
        return (realToFrac ddpi, realToFrac hdpi, realToFrac vdpi)

-- | Returns the default resize factor for Windows
getWindowsFactor :: IO Double
getWindowsFactor = getDisplayDPIFactor

{-|
Returns a scale factor to handle HiDPI on Linux.

Assumes that resolutions larger than 1920 belong to HiDPI displays and scales
them by 2. In case this is not an appropriate scale, 'appScaleFactor' can be
used to set a custom scaling factor, while 'appDisableAutoScale' can be used for
disabling automatic detection.

There is not a practical DPI/scale detection solution that works for all
combinations of Linux display servers and window managers. For a reference of
some of the existing options for detection, check here:
https://wiki.archlinux.org/title/HiDPI.
-}
getLinuxFactor :: IO Double
getLinuxFactor = do
  dpiFactor <- getDisplayDPIFactor

  alloca $ \pmode -> do
    Raw.getCurrentDisplayMode 0 pmode
    mode <- peek pmode

    let width = Raw.displayModeW mode
    let detectedDPI
          | dpiFactor > 0 = dpiFactor
          | width <= 1920 = 1
          | otherwise = 2

    return detectedDPI

-- | Returns DPI scaling factor using SDL_GetDisplayDPI
getDisplayDPIFactor :: IO Double
getDisplayDPIFactor = do
  (ddpi, hdpi, vdpi) <- getDisplayDPI
  return (hdpi / 96)

setDisableCompositorHint :: Bool -> IO ()
setDisableCompositorHint disable = void $
  withCString "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" $ \cHintNameStr ->
    withCString disableStr $ \cDisableStr ->
      Raw.setHint cHintNameStr cDisableStr
  where
    disableStr = if disable then "1" else "0"
