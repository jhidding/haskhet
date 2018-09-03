{-# LANGUAGE LambdaCase #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller (Action, subscriptions, update) where

import Model
import Game
import qualified Data.Map.Strict as Map
-- | Handle one iteration of the game

import qualified Helm
import qualified Helm.Cmd as Cmd
import qualified Helm.Sub as Sub
import qualified Helm.Keyboard as Keyboard

data Action =
      DoNothing
    | MoveCursor  Orientation
    | RotateAsset Twist
    | NextPhase

moveCursor :: Orientation -> Position -> Position
moveCursor North (x, y) = (x, (y + 1) `min` 7)
moveCursor West  (x, y) = ((x - 1) `max` 0, y)
moveCursor South (x, y) = (x, (y - 1) `max` 0)
moveCursor East  (x, y) = ((x + 1) `min` 9, y)

updateCursor :: (Position -> Position) -> GameState -> GameState
updateCursor f s@GameState{selection=sel} = s { selection = f sel }

subscriptions :: Helm.Engine e => Helm.Sub e Action
subscriptions = Sub.batch
    [ Keyboard.downs $ \case
        Keyboard.UpKey       -> MoveCursor North
        Keyboard.LeftKey     -> MoveCursor West
        Keyboard.DownKey     -> MoveCursor South
        Keyboard.RightKey    -> MoveCursor East
        Keyboard.ReturnKey   -> NextPhase
        Keyboard.PageDownKey -> RotateAsset TwistRight
        Keyboard.PageUpKey   -> RotateAsset TwistLeft
        _                    -> DoNothing
    ]

update :: Helm.Engine e => Model -> Action -> (Model, Helm.Cmd e Action)
update model (MoveCursor d) = 
  ( updateCursor (moveCursor d) model
  , Cmd.none)
update model _ = (model, Cmd.none)
