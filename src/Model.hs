-- | This module contains the data types
--   which represent the state of the game
module Model where

import Game

data GameState = GameState {
                   board       :: Board
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState classicBoard 0