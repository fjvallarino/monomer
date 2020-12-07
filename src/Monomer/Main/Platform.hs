module Monomer.Main.Platform where
{- HLint Ignore: Use forM_ -}

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

defaultWindowSize :: (Int, Int)
defaultWindowSize = (640, 480)

defaultUseHdpi :: Bool
defaultUseHdpi = True

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

detroySDLWindow :: SDL.Window -> IO ()
detroySDLWindow window = do
  putStrLn "About to destroyWindow"
  SDL.destroyWindow window
  Raw.quitSubSystem Raw.SDL_INIT_VIDEO
  SDL.quit

getCurrentMousePos :: (MonadIO m) => m Point
getCurrentMousePos = do
  SDL.P (SDL.V2 x y) <- Mouse.getAbsoluteMouseLocation
  return $ Point (fromIntegral x) (fromIntegral y)

getDrawableSize :: (MonadIO m) => SDL.Window -> m Size
getDrawableSize window = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  return $ Size (fromIntegral fbWidth) (fromIntegral fbHeight)

getWindowSize :: (MonadIO m) => SDL.Window -> Double -> m Size
getWindowSize window dpr = do
  Size rw rh <- getDrawableSize window

  return $ Size (rw / dpr) (rh / dpr)

getPlatform :: (MonadIO m) => m Text
getPlatform = do
  platform <- liftIO . peekCString =<< Raw.getPlatform

  return $ T.pack platform

getKeyCode :: String -> Maybe KeyCode
getKeyCode name = unsafePerformIO $ withCString name getKeyCodeFFI where
  getKeyCodeFFI cname = fmap convert (Raw.getKeyFromName cname)
  convert Raw.SDLK_UNKNOWN = Nothing
  convert code = Just (KeyCode $ fromIntegral code)
