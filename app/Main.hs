module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Game
import PaintBoard

-- .Interface.IO.Game

-- main :: IO ()
-- main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
--               black            -- Background color
--               10               -- Frames per second
--               initialState     -- Initial state
--               view             -- View function
--               input            -- Event function
--               step             -- Step function

main :: IO ()
main = do
    display (InWindow "Khet" (100, 100) (800, 800))
            black
            $ paintBoard classicBoard