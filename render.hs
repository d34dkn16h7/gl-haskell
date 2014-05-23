module Render where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Data

init :: Int -> Int -> ((GLFW.Window,GameWorld) -> IO a) -> IO a -> IO a
init w h rn err = do
  mw <- GLFW.createWindow w h "(opengl | haskell) -> YAY!!" Nothing Nothing
  case mw of
    Nothing ->
    	  err
    Just m  -> do
          GLFW.makeContextCurrent mw
          GL.clearColor $= Color4 1 0 0 0
          rn (m , GameWorld m (Camera (Vec2 0 0) (Vec2 (realToFrac w) (realToFrac h))))

update :: Camera -> IO ()
update camera = do
  GL.loadIdentity
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.translate $ GL.Vector3 (gX' camera) (gY' camera) 0.0
  GL.renderPrimitive GL.Quads $ draw camera

draw :: Camera -> IO ()
draw cam = do
  let _scr = _screen cam
  let ratio = _y _scr / _x _scr
  print ratio
  renderQuad ratio (Vec2 1 0) (Vec2 1 1)
  renderQuad ratio (Vec2 (-1) 0) (Vec2 0.2 0.2)

renderQuad :: GLfloat -> Vec2 -> Vec2 -> IO ()
renderQuad ratio (Vec2 x y) (Vec2 sx sy) = do
    GL.color $ Color4 1 1 1 (1 :: GLfloat)
    glVertex2 ((-0.5 + x) * sx * ratio) ((-0.5 + y) * sy * ratio) 
    glVertex2 ((-0.5 + x) * sx * ratio) ((0.5 + y) * sy * ratio)
    GL.color $ Color4 0.5 0.5 0.5 (0.5 :: GLfloat)
    glVertex2 ((0.5 + x) * sx * ratio) ((0.5 + y) * sy * ratio)
    glVertex2 ((0.5 + x) * sx * ratio) ((-0.5 + y) * sy * ratio)

glVertex2 :: GLfloat -> GLfloat -> IO ()
glVertex2 x y = glVertex3 x y 0

glVertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
glVertex3 x y z = GL.vertex $ GL.Vertex3 x y z