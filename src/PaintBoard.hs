module PaintBoard where

import Game
import Graphics.Gloss

paintSquare :: Picture
paintSquare = color orange $ rectangleSolid 0.9 0.9

paintBackground :: Picture
paintBackground = 
    pictures [translate (fromIntegral x) (fromIntegral y) paintSquare
             | x <- [0..9], y <- [0..7]]

paintBoard :: Board -> Picture
paintBoard board = scale 100.0 100.0 paintBackground