module Input where

import qualified Graphics.UI.GLFW as GLFW
import Data

uInput :: GLFW.Window -> IO Input
uInput m_win = do
  left  <- GLFW.getKey m_win GLFW.Key'Left
  right <- GLFW.getKey m_win GLFW.Key'Right
  up    <- GLFW.getKey m_win GLFW.Key'Up
  down  <- GLFW.getKey m_win GLFW.Key'Down
  return $ Input left right up down

update :: GameWorld -> IO GameWorld
update GameWorld {win = m_win , cam = m_cam} = do
  _input <- Input.uInput m_win
  let x = gX' m_cam
  let y = gY' m_cam
  let nX = if (left _input) == GLFW.KeyState'Pressed then x + 0.001 else if (right _input) == GLFW.KeyState'Pressed then x - 0.001 else x 
  let nY = if (down _input) == GLFW.KeyState'Pressed then y + 0.001 else if (up _input) == GLFW.KeyState'Pressed then y - 0.001 else y 
  return  (GameWorld m_win $ (Camera $ Vec2 nX nY))