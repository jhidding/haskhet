-- | This module defines how to turn
--   the game state into a picture
module View (view) where

import Model
import Game
import qualified Data.Map.Strict as Map

import Linear.V2 (V2(V2))
import Linear.V3 (V3(V3))
import Linear.Matrix
import qualified Helm
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Graphics2D.Transform as Transform
import Helm.Graphics2D
import Helm.Color

import View.Colors
import View.Pieces

tile :: Helm.Engine e => Bool -> Form e
tile sel = group
    [ move (V2 0.05 (- 0.05)) $ filled shadowColor $ rect (V2 0.92 0.92)
    , gradient (tileGradient c tileColor2)  $ rect (V2 0.92 0.92)
    ] where c = if sel then selectedTileColor else tileColor1

viewBackground :: Helm.Engine e => Model -> Form e
viewBackground GameState { selection = sel } = group
    [ move (V2 (fromIntegral i) (fromIntegral j)) $ tile (sel == (i, j))
    | i <- [0..9], j <- [0..7] ]

flipY :: Helm.Engine e => [Form e] -> Form e
flipY = groupTransform $ Transform (V3 (V3 100.0    0.0   50.0)
                                       (V3 0.0 (- 100.0) 750.0)
                                       (V3 0.0      0.0    0.0))

view :: Helm.Engine e => Model -> Helm.Graphics e
view m@GameState{ board = board } = Helm.Graphics2D $ collage
    [  move (V2 500.0 425.0) $ filled backgroundColor $ rect (V2 1000.0 850.0)
    , flipY $
        viewBackground m : [
            let pal = if c == White then whitePalette else redPalette
                role = case p of
                    Anubis  -> anubis
                    Scarab  -> scarab
                    Pyramid -> pyramid
                    Pharaoh -> pharaoh
                    _       -> []
            in move (V2 (fromIntegral i * 1.25) (fromIntegral j * 1.25)) $
               scale 0.8 $ drawPiece pal pieceBase role o
            | ((i, j), Asset p o c) <- Map.toList board ]
    ]
