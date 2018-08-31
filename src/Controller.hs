-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Game
import qualified Data.Map.Strict as Map

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs = return 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = do
    print e
    return (inputKey e gstate)

moveSelection :: Orientation -> Position -> Position
moveSelection North (x, y) = (x, (y + 1) `min` 7)
moveSelection West  (x, y) = ((x - 1) `max` 0, y)
moveSelection South (x, y) = (x, (y - 1) `max` 0)
moveSelection East  (x, y) = ((x + 1) `min` 9, y)

updateSelection :: (Position -> Position) -> GameState -> GameState
updateSelection f s@GameState{selection=sel} = s { selection = f sel }

keyAction :: SpecialKey -> GameState -> GameState
keyAction KeyUp    = updateSelection (moveSelection North)
keyAction KeyLeft  = updateSelection (moveSelection West) 
keyAction KeyDown  = updateSelection (moveSelection South)
keyAction KeyRight = updateSelection (moveSelection East) 
keyAction KeyEnter = cyclePhase
keyAction _ = id

inputKey :: Event-> GameState -> GameState
inputKey (EventKey (SpecialKey k) Down _ _) gstate
  = keyAction k gstate
inputKey _ gstate = gstate -- Otherwise keep the same