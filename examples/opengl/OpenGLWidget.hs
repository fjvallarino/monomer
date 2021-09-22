{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module OpenGLWidget (
  openGLWidget
) where

import Control.Lens ((&), (^.), (.~))
import Control.Monad
import Data.Default
import Data.Typeable (cast)
import Data.Vector.Storable (Vector)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import qualified Data.Vector.Storable as V

import Monomer
import Monomer.Widgets.Single

import qualified Monomer.Lens as L

data OpenGLWidgetMsg
  = OpenGLWidgetInit GLuint (Ptr GLuint) (Ptr GLuint)
  deriving (Show, Eq)

data OpenGLWidgetState = OpenGLWidgetState {
  _ogsLoaded :: Bool,
  _ogsShaderId :: GLuint,
  _ogsVao :: Ptr GLuint,
  _ogsVbo :: Ptr GLuint
} deriving (Show, Eq)

openGLWidget :: Color -> WidgetNode s e
openGLWidget color = defaultWidgetNode "openGLWidget" widget where
  widget = makeOpenGLWidget color state
  state = OpenGLWidgetState False 0 nullPtr nullPtr

makeOpenGLWidget :: Color -> OpenGLWidgetState -> Widget s e
makeOpenGLWidget color state = widget where
  widget = createSingle state def {
    singleInit = init,
    singleMerge = merge,
    singleDispose = dispose,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  init wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    buffers = 2

    initOpenGL = do
      -- This needs to run in render thread
      program <- createShaderProgram
      vaoPtr <- malloc
      vboPtr <- malloc
      glGenVertexArrays buffers vaoPtr
      glGenBuffers buffers vboPtr

      return $ OpenGLWidgetInit program vaoPtr vboPtr

    reqs = [RunInRenderThread widgetId path initOpenGL]

  merge wenv node oldNode oldState = resultNode newNode where
    newNode = node
      & L.widget .~ makeOpenGLWidget color oldState

  dispose wenv node = resultReqs node reqs where
    OpenGLWidgetState _ shaderId vaoPtr vboPtr = state
    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    buffers = 2

    disposeOpenGL = do
      -- This needs to run in render thread
      glDeleteProgram shaderId
      glDeleteVertexArrays buffers vaoPtr
      glDeleteBuffers buffers vboPtr
      free vaoPtr
      free vboPtr

    reqs = [RunInRenderThread widgetId path disposeOpenGL]

  handleMessage wenv node target msg = case cast msg of
    Just (OpenGLWidgetInit shaderId vao vbo) -> Just (resultNode newNode) where
      newState = OpenGLWidgetState True shaderId vao vbo
      newNode = node
        & L.widget .~ makeOpenGLWidget color newState
    _ -> Nothing

  getSizeReq wenv node = (sizeReqW, sizeReqH) where
    sizeReqW = expandSize 100 1
    sizeReqH = expandSize 100 1

  render wenv node renderer =
    when (_ogsLoaded state) $
      createRawTask renderer $
        doInScissor winSize dpr offset activeVp $
          drawVertices state (toVectorVAO winSize offset color triangle)
    where
      dpr = wenv ^. L.dpr
      winSize = wenv ^. L.windowSize
      activeVp = wenv ^. L.viewport
      offset = wenv ^. L.offset

      -- Simple triangle
      style = currentStyle wenv node
      nodeVp = getContentArea node style

      Rect rx ry rw rh = nodeVp
      triangle = [(rx + rw, ry + rh), (rx, ry + rh), (rx + rw / 2, ry)]

doInScissor :: Size -> Double -> Point -> Rect -> IO () -> IO ()
doInScissor winSize dpr offset vp action = do
  glEnable GL_SCISSOR_TEST
  -- OpenGL's Y axis increases from bottom to top
  glScissor (round (rx + ox)) (round $ winH - ry - oy - rh) (round rw) (round rh)
  action
  glDisable GL_SCISSOR_TEST
  where
    winH = winSize ^. L.h * dpr
    Point ox oy = mulPoint dpr offset
    Rect rx ry rw rh = mulRect dpr vp

toVectorVAO :: Size -> Point -> Color -> [(Double, Double)] -> Vector Float
toVectorVAO (Size w h) (Point ox oy) (Color r g b a) points = vec where
  px x = realToFrac $ (x + ox - w / 2) / (w / 2)
  -- OpenGL's Y axis increases from bottom to top
  py y = realToFrac $ (h / 2 - y - oy) / (h / 2)
  col c = realToFrac (fromIntegral c / 255)
  row (x, y) = [px x, py y, 0, col r, col g, col b]
  vec = V.fromList . concat $ row <$> points

drawVertices
  :: forall a . Storable a
  => OpenGLWidgetState
  -> Vector a
  -> IO ()
drawVertices state vertices = do
  vao <- peek vaoPtr
  vbo <- peek vboPtr

  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo

  -- Copies raw data from vector to OpenGL memory
  V.unsafeWith vertices $ \vertsPtr ->
    glBufferData
      GL_ARRAY_BUFFER
      (fromIntegral (V.length vertices * floatSize))
      (castPtr vertsPtr)
      GL_STATIC_DRAW

  -- The vertex shader expects two arguments. Position:
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral (floatSize * 6)) nullPtr
  glEnableVertexAttribArray 0

  -- Color:
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (fromIntegral (floatSize * 6)) (nullPtr `plusPtr` (floatSize * 3))
  glEnableVertexAttribArray 1

  glUseProgram shaderId
  glBindVertexArray vao
  glDrawArrays GL_TRIANGLES 0 3

  where
    floatSize = sizeOf (undefined :: Float)
    OpenGLWidgetState _ shaderId vaoPtr vboPtr = state

createShaderProgram :: IO GLuint
createShaderProgram = do
  shaderProgram <- glCreateProgram
  vertexShader <- compileShader GL_VERTEX_SHADER "examples/opengl/shaders/vert.glsl"
  fragmentShader <- compileShader GL_FRAGMENT_SHADER "examples/opengl/shaders/frag.glsl"

  glAttachShader shaderProgram vertexShader
  glAttachShader shaderProgram fragmentShader

  glLinkProgram shaderProgram
  checkProgramLink shaderProgram

  glDeleteShader vertexShader
  glDeleteShader fragmentShader

  return shaderProgram

compileShader :: GLenum -> FilePath -> IO GLuint
compileShader shaderType shaderFile = do
  shader <- glCreateShader shaderType
  shaderSource <- readFile shaderFile >>= newCString

  alloca $ \shadersStr -> do
    shadersStr `poke` shaderSource
    glShaderSource shader 1 shadersStr nullPtr
    glCompileShader shader
    checkShaderCompile shader

  return shader

checkProgramLink :: GLuint -> IO ()
checkProgramLink programId = do
  alloca $ \successPtr -> do
    alloca $ \infoLogPtr -> do
      glGetProgramiv programId GL_LINK_STATUS successPtr
      success <- peek successPtr

      when (success <= 0) $ do
        glGetProgramInfoLog programId 512 nullPtr infoLogPtr
        putStrLn =<< peekCString infoLogPtr

checkShaderCompile :: GLuint -> IO ()
checkShaderCompile shaderId = do
  alloca $ \successPtr ->
    alloca $ \infoLogPtr -> do
      glGetShaderiv shaderId GL_COMPILE_STATUS successPtr
      success <- peek successPtr

      when (success <= 0) $ do
        glGetShaderInfoLog shaderId 512 nullPtr infoLogPtr
        putStrLn "Failed to compile shader "
