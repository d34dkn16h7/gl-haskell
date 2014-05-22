module Main where

import System.Exit
import qualified Graphics.UI.GLFW as GLFW

import Data
import qualified Input
import qualified Render

main = do
  GLFW.init
  Render.init 700 500 run err

run :: (GLFW.Window , GameWorld) -> IO ()
run (_window , world) = do
  world <- Input.update world
  Render.update $ cam world
  GLFW.swapBuffers _window
  GLFW.pollEvents
  esc <- isEscape _window
  wsClose <- GLFW.windowShouldClose _window
  runIfTrue esc wsClose GLFW.terminate (run (_window,world))

err =  do
  print "Can't create window"
  exitWith (ExitFailure 0)

isEscape _window = do
  esc <- GLFW.getKey _window GLFW.Key'Escape
  return (esc == GLFW.KeyState'Pressed)

runIfTrue v1 v2 fail succ = if v1 || v2 then fail else succ
