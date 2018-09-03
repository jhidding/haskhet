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
    , phase       :: GamePhase
    , player      :: Colour
    }

type Model = GameState

cyclePhase :: GameState -> GameState
cyclePhase g@GameState { phase = LaserFiring, player = player } =
    g { phase = MoveSelection, player = rotateEnum 1 player }
cyclePhase g@GameState { phase = MoveSelection } =
    g { phase = LaserFiring }

initialState :: GameState
initialState = GameState classicBoard (0, 0) MoveSelection White