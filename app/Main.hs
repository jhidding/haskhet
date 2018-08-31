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
    let (path, c) = fireRay classicBoard $ forward (Ray (9, 0) North)
    p <- paintBoard classicBoard
    let p' = pictures [p, color magenta (paintPath $ (9, 0) : path)]
    putStrLn $ "computed path: " ++ show path ++ " and " ++ show c
    display (InWindow "Khet" (1000, 800) (100, 100))
            black p'