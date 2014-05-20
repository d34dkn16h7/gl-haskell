import Data.Maybe
import System.Exit
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

{- data stuff -}
data V2 = V2 { x :: GLfloat, y :: GLfloat}
data Camera = Camera {pos :: V2}
data GameWorld = GameWorld { win :: GLFW.Window , cam :: Camera }

gX' (Camera p) = x p
gY' (Camera p) = y p
gV2 Camera {pos = p} = p

main = do
  GLFW.init
  mw <- GLFW.createWindow 400 400 "(opengl | haskell) -> YAY!!" Nothing Nothing
  case mw of
    Nothing -> do
          print "Can't create window"
          exitWith (ExitFailure 0)
    Just m  -> do
          GLFW.makeContextCurrent mw
          GL.clearColor $= Color4 1 0 0 0
          run m (GameWorld m $ (Camera $ V2 0 0))

isEscape mw = do
  esc <- GLFW.getKey mw GLFW.Key'Escape
  return (esc == GLFW.KeyState'Pressed)

runIfTrue v1 v2 fail succ = if v1 || v2 then fail else succ

run :: GLFW.Window -> GameWorld -> IO ()
run mw world = do
  world <- gInput world
  render $ cam world
  GLFW.swapBuffers mw
  GLFW.pollEvents
  esc <- isEscape mw
  wsClose <- GLFW.windowShouldClose mw
  runIfTrue esc wsClose GLFW.terminate (run mw world)

gInput :: GameWorld -> IO GameWorld
gInput GameWorld {win = m_win , cam = m_cam} = do
  left  <- GLFW.getKey m_win GLFW.Key'Left
  right <- GLFW.getKey m_win GLFW.Key'Right
  up    <- GLFW.getKey m_win GLFW.Key'Up
  down  <- GLFW.getKey m_win GLFW.Key'Down
  let x = gX' m_cam
  let y = gY' m_cam
  let nX = if left == GLFW.KeyState'Pressed then x - 0.001 else if right == GLFW.KeyState'Pressed then x + 0.001 else x 
  let nY = if down == GLFW.KeyState'Pressed then y - 0.001 else if up == GLFW.KeyState'Pressed then y + 0.001 else y 
  return  (GameWorld m_win $ (Camera $ V2 nX nY))

render :: Camera -> IO ()
render camera = do
  GL.loadIdentity
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.translate $ GL.Vector3 (gX' camera) (gY' camera) (0.0 :: GLfloat)
  GL.renderPrimitive GL.Quads $ draw 

draw = do
  renderQuad 1 0
  renderQuad (-1) 0

renderQuad x y = do
    GL.color $ Color4 1 1 1 (1 :: GLfloat)
    GL.vertex $ GL.Vertex2 (-0.5 + x) (-0.5 + y :: GLfloat)
    GL.vertex $ GL.Vertex2 (-0.5 + x) (0.5 + y :: GLfloat)
    GL.color $ Color4 0.5 0.5 0.5 (0.5 :: GLfloat)
    GL.vertex $ GL.Vertex2 (0.5 + x) (0.5 + y :: GLfloat)
    GL.vertex $ GL.Vertex2 (0.5 + x) (-0.5 + y :: GLfloat)