module Data where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data Input = Input { left :: GLFW.KeyState , right :: GLFW.KeyState , up :: GLFW.KeyState , down :: GLFW.KeyState}
data Vec2 = Vec2 { _x :: GLfloat, _y :: GLfloat}
data Camera = Camera {_pos :: Vec2}
data GameWorld = GameWorld { win :: GLFW.Window , cam :: Camera }

{- f For easy access -}

gX' :: Camera -> GLfloat
gX' cam = _x $ _pos cam

gY' :: Camera -> GLfloat
gY' cam = _y $ _pos cam