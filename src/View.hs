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

paintStatus :: GameState -> Picture
paintStatus GameState { phase = phase, player = player } =
    pictures
        [ color yellow
            $ text $ "turn: " ++ show player ++ " -- " ++ show phase
        , color green $ translate 0 150
            $ text $ "cursor keys to move, space: select, "
                  ++ "pgUp/pgDn: rotate, enter: fire"
        ]

view :: GameState -> IO Picture
view g@GameState{selection=selection, board=board} = do
    let (path, c) = fireRay board $ forward (Ray (9, 0) North)
    p <- paintBoard board
    let p' = scale 100.0 100.0
           $ translate (- 4.5) (- 3.25)
           $ pictures [
                p, color magenta (paintPath $ (9, 0) : path),
                paintSelector selection ]
    let s' = translate (- 490) (-415) $ scale 0.15 0.15 $ paintStatus g
    return $ pictures [ p', s' ]

viewPure :: GameState -> Picture
viewPure gstate = blank