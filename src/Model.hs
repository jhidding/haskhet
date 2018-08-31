-- | This module contains the data types
--   which represent the state of the game
module Model where

import Game

data GamePhase
    = MoveSelection
    | LaserFiring
    deriving (Show, Enum, Bounded)

data GameState = GameState
    { board       :: Board
    , selection   :: Position
    , animations  :: [Animation]
    , phase       :: GamePhase
    , player      :: Colour
    }

initialState :: GameState
initialState = GameState classicBoard (0, 0) [] MoveSelection White