{-|
Module      : Monomer.Main.Platform
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for SDL platform related operations.
-}
module Monomer.Main.Platform (
  defaultWindowSize,
  defaultUseHdpi,
  initSDLWindow,
  detroySDLWindow,
  getCurrentMousePos,
  getDrawableSize,
  getWindowSize,
  getPlatform
) where

import Control.Monad.State
import Data.Maybe
import Data.Text (Text)
import Foreign.C (peekCString, withCString)
import Foreign.C.Types
import SDL (($=))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T
import qualified Foreign.C.String as STR
import qualified SDL
import qualified SDL.Input.Mouse as Mouse
import qualified SDL.Raw as Raw
import qualified SDL.Raw.Error as SRE

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Main.Types
import Monomer.Event.Types
import Monomer.Widgets.Composite

foreign import ccall unsafe "initGlew" glewInit :: IO CInt

-- | Default window size if not is specified.
defaultWindowSize :: (Int, Int)
defaultWindowSize = (640, 480)

-- | Default use of HDPI or not.
defaultUseHdpi :: Bool
defaultUseHdpi = True

-- | Creates and initializes a window using the provided configuration.
initSDLWindow :: AppConfig e -> IO (SDL.Window, Double)
initSDLWindow config = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {
        SDL.windowInitialSize = SDL.V2 (fromIntegral winW) (fromIntegral winH),
        SDL.windowHighDPI = windowHiDPI,
        SDL.windowResizable = windowResizable,
        SDL.windowBorder = windowBorder,
        SDL.windowGraphicsContext = SDL.OpenGLContext customOpenGL
      }

  -- Get device pixel rate
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  let dpr = fromIntegral fbWidth / fromIntegral winW

  when (isJust (_apcWindowTitle config)) $
    SDL.windowTitle window $= fromJust (_apcWindowTitle config)

  when windowFullscreen $
    SDL.setWindowMode window SDL.FullscreenDesktop

  when windowMaximized $
    SDL.setWindowMode window SDL.Maximized

  err <- SRE.getError
  err <- STR.peekCString err
  putStrLn err

  _ <- SDL.glCreateContext window

  _ <- glewInit

  return (window, dpr)
  where
    customOpenGL = SDL.OpenGLConfig {
      SDL.glColorPrecision = SDL.V4 8 8 8 0,
      SDL.glDepthPrecision = 24,
      SDL.glStencilPrecision = 8,
      --SDL.glProfile = SDL.Core SDL.Debug 3 2,
      SDL.glProfile = SDL.Core SDL.Normal 3 2,
      SDL.glMultisampleSamples = 1
    }
    (winW, winH) = case _apcWindowState config of
      Just (MainWindowNormal size) -> size
      _ -> defaultWindowSize
    windowResizable = fromMaybe True (_apcWindowResizable config)
    windowBorder = fromMaybe True (_apcWindowBorder config)
    windowHiDPI = fromMaybe defaultUseHdpi (_apcHdpi config)
    windowFullscreen = case _apcWindowState config of
      Just MainWindowFullScreen -> True
      _ -> False
    windowMaximized = case _apcWindowState config of
      Just MainWindowMaximized -> True
      _ -> False

-- | Destroys the provided window, shutdowns the video subsystem and SDL.
detroySDLWindow :: SDL.Window -> IO ()
detroySDLWindow window = do
  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  Raw.quitSubSystem Raw.SDL_INIT_VIDEO
  SDL.quit

-- | Returns the current mouse position.
getCurrentMousePos :: (MonadIO m) => m Point
getCurrentMousePos = do
  SDL.P (SDL.V2 x y) <- Mouse.getAbsoluteMouseLocation
  return $ Point (fromIntegral x) (fromIntegral y)

-- | Returns the drawable size of the provided window. May differ from window
-- | size if HDPI is enabled.
getDrawableSize :: (MonadIO m) => SDL.Window -> m Size
getDrawableSize window = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  return $ Size (fromIntegral fbWidth) (fromIntegral fbHeight)

-- | Returns the size of the provided window.
getWindowSize :: (MonadIO m) => SDL.Window -> Double -> m Size
getWindowSize window dpr = do
  Size rw rh <- getDrawableSize window

  return $ Size (rw / dpr) (rh / dpr)

-- | Returns the name of the host OS.
getPlatform :: (MonadIO m) => m Text
getPlatform = do
  platform <- liftIO . peekCString =<< Raw.getPlatform

  return $ T.pack platform
