module Main where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Data
import qualified Input
import qualified Render

main = do
  GLFW.init
  Render.init $ run

run :: GLFW.Window -> GameWorld -> IO ()
run mw world = do
  world <- Input.update world
  Render.update $ cam world
  GLFW.swapBuffers mw
  GLFW.pollEvents
  esc <- isEscape mw
  wsClose <- GLFW.windowShouldClose mw
  runIfTrue esc wsClose GLFW.terminate (run mw world)

isEscape mw = do
  esc <- GLFW.getKey mw GLFW.Key'Escape
  return (esc == GLFW.KeyState'Pressed)

runIfTrue v1 v2 fail succ = if v1 || v2 then fail else succ
