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
          rn (m , GameWorld m $ Camera $ Vec2 0 0)

update :: Camera -> IO ()
update camera = do
  GL.loadIdentity
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.translate $ GL.Vector3 (gX' camera) (gY' camera) (0.0 :: GLfloat)
  GL.renderPrimitive GL.Quads $ draw 

draw :: IO ()
draw = do
  renderQuad $ Vec2 1 0
  renderQuad $ Vec2 (-1) 0

glVertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
glVertex3 x y z = GL.vertex $ GL.Vertex3 x y z

glVertex2 :: GLfloat -> GLfloat -> IO ()
glVertex2 x y = glVertex3 x y 0

renderQuad :: Vec2 -> IO ()
renderQuad (Vec2 x y) = do
    GL.color $ Color4 1 1 1 (1 :: GLfloat)
    glVertex2 (-0.5 + x) (-0.5 + y)
    glVertex2 (-0.5 + x) (0.5 + y)
    GL.color $ Color4 0.5 0.5 0.5 (0.5 :: GLfloat)
    glVertex2 (0.5 + x) (0.5 + y)
    glVertex2 (0.5 + x) (-0.5 + y)