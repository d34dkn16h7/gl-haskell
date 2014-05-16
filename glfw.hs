import Data.Maybe
import System.Exit
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G

main = do
  G.init
  mw <- G.createWindow 400 400 "title" Nothing Nothing
  case mw of
    Nothing -> do
          err "Can't create window"
          exitWith (ExitFailure 0)
    Just m  -> do
          G.makeContextCurrent mw
          GL.clearColor $= Color4 1 0 0 0
          G.setWindowCloseCallback m (Just exit')
          start m
	
exit' w = exitWith ExitSuccess

err e = print e

start :: G.Window -> IO ()
start mw = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  render
  G.swapBuffers mw
  GL.flush
  G.pollEvents
  esc <- G.getKey mw G.Key'Escape
  if esc == G.KeyState'Pressed
  then G.terminate
  else start mw

render =
  GL.renderPrimitive GL.Points $ do
    GL.vertex $ GL.Vertex2 (0.0 :: GLfloat) (0.0 :: GLfloat)
    GL.vertex $ GL.Vertex2 (0.5 :: GLfloat) (0.0 :: GLfloat)
    GL.vertex $ GL.Vertex2 (-0.5 :: GLfloat) (0.0 :: GLfloat)