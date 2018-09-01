-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Game
import qualified Data.Map.Strict as Map
-- | Handle one iteration of the game

moveSelection :: Orientation -> Position -> Position
moveSelection North (x, y) = (x, (y + 1) `min` 7)
moveSelection West  (x, y) = ((x - 1) `max` 0, y)
moveSelection South (x, y) = (x, (y - 1) `max` 0)
moveSelection East  (x, y) = ((x + 1) `min` 9, y)

updateSelection :: (Position -> Position) -> GameState -> GameState
updateSelection f s@GameState{selection=sel} = s { selection = f sel }

