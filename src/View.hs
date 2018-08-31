-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Game
import PaintBoard

paintSelector :: Position -> Picture
paintSelector (x, y) = translate (fromIntegral x)
                                 (fromIntegral y)
                     $ color yellow $ rectangleWire 0.9 0.9

view :: GameState -> IO Picture
view (GameState board selector) = do
    let (path, c) = fireRay board $ forward (Ray (9, 0) North)
    p <- paintBoard board
    let p' = scale 100.0 100.0
           $ translate (- 4.5) (- 3.25)
           $ pictures [
                p, color magenta (paintPath $ (9, 0) : path),
                paintSelector selector ]
    return p'

viewPure :: GameState -> Picture
viewPure gstate = blank