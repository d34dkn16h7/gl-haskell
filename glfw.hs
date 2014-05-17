import Data.Maybe
import System.Exit
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

main = do
  GLFW.init
  mw <- GLFW.createWindow 400 400 "title" Nothing Nothing
  case mw of
    Nothing -> do
          err "Can't create window"
          exitWith (ExitFailure 0)
    Just m  -> do
          GLFW.makeContextCurrent mw
          GL.clearColor $= Color4 1 0 0 0
          run m

err e = print e

isEscape mw = do
  esc <- GLFW.getKey mw GLFW.Key'Escape
  return (esc == GLFW.KeyState'Pressed)

runIfTrue v1 v2 fail succ = if v1 || v2 then fail else succ

run :: GLFW.Window -> IO ()
run mw = do
  render
  GLFW.swapBuffers mw
  GLFW.pollEvents
  esc <- isEscape mw
  wsClose <- GLFW.windowShouldClose mw
  runIfTrue esc wsClose GLFW.terminate (run mw)

render = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.renderPrimitive GL.Lines $ do
    GL.vertex $ GL.Vertex2 (-0.5 :: GLfloat) (0.0 :: GLfloat)
    GL.vertex $ GL.Vertex2 (0.0 :: GLfloat) (0.0 :: GLfloat)
    GL.vertex $ GL.Vertex2 (0.5 :: GLfloat) (0.0 :: GLfloat)
    GL.vertex $ GL.Vertex2 (0.5 :: GLfloat) (0.5 :: GLfloat)
