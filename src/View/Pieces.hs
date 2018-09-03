module View.Pieces where

import Model
import Game
import View.Colors
import qualified Data.Map.Strict as Map

import Linear.V2 (V2(V2))
import Linear.V3 (V3(V3))
import Linear.Matrix
import Helm.Graphics2D

cyclePairs :: [a] -> [(a, a)]
cyclePairs [] = []
cyclePairs (fst:xs) = cyclePairs' (fst:xs)
    where cyclePairs' [x1]       = [(x1, fst)]
          cyclePairs' (x1:x2:xs) = (x1, x2) : cyclePairs' (x2:xs)

rotateShade :: Shade -> Orientation -> Shade
rotateShade s o
    | si < 4    = toEnum $ (si + oi) `mod` 4
    | otherwise = s
    where si = fromEnum s
          oi = fromEnum o

rotateMatrix :: Orientation -> M22 Double
rotateMatrix North = V2 (V2    1.0    0.0)  (V2    0.0    1.0)
rotateMatrix West  = V2 (V2    0.0 (- 1.0)) (V2    1.0    0.0)
rotateMatrix South = V2 (V2 (- 1.0)   0.0)  (V2    0.0 (- 1.0))
rotateMatrix East  = V2 (V2    0.0    1.0)  (V2 (- 1.0)   0.0)

rotatePiece :: Orientation -> [(Path, Shade)] -> [(Path, Shade)]
rotatePiece o ps = [(Path [rotateMatrix o !* (q - V2 0.5 0.5) | q <- p], rotateShade s o) | (Path p, s) <- ps]

pieceBase :: [(Path, Shade)]
pieceBase =
    (path $ zipWith (+) k l, FromOver) :
    [ (path [k1, k1+l1, k2+l2, k2], c)
    | (c, (k1, k2), (l1, l2)) <- zip3 shades (cyclePairs k) (cyclePairs l)
    ]
    where k = [V2 0.0 0.0, V2  1.0 0.0, V2  1.0  1.0, V2 0.0  1.0]
          l = [V2   m   m, V2 (-m)   m, V2 (-m) (-m), V2   m (-m)]
          m = 0.05

ngonPath :: Int -> V2 Double -> Double -> Double -> Path
ngonPath n p r phi =
    path [ let a = phi + 2 * pi * fromIntegral i / fromIntegral n
           in p + V2 r r * V2 (cos a) (sin a)
         | i <- [0 .. (n-1)] ]

anubis :: [(Path, Shade)]
anubis =
    [ ( ngonPath 6 (V2 0.2 0.25) 0.15 (pi / 6), FromBottom )
    , ( ngonPath 6 (V2 0.8 0.25) 0.15 (pi / 6), FromBottom )
    , ( path [V2 0.2 0.1, V2 0.8 0.1, V2 0.8 0.4, V2 0.2 0.4], FromRight )
    , ( path [V2 0.5 0.25, V2 0.35 0.01, V2 0.45 0.9,
              V2 0.55 0.9, V2 0.65 0.01], FromLeft )
    , ( ngonPath 4 (V2 0.4 0.4) 0.05 0.0, FromBlack )
    , ( ngonPath 4 (V2 0.6 0.4) 0.05 0.0, FromBlack )
    ]

pharaoh :: [(Path, Shade)]
pharaoh =
    [ ( ngonPath 6 (V2 0.2 0.25) 0.15 (pi / 6), FromBottom )
    , ( ngonPath 6 (V2 0.8 0.25) 0.15 (pi / 6), FromBottom )
    , ( path [V2 0.2 0.2, V2 0.45 0.2, V2 0.45 0.8, V2 0.2 0.8], FromRight )
    , ( path [V2 0.8 0.2, V2 0.55 0.2, V2 0.55 0.8, V2 0.8 0.8], FromRight )
    , ( path [V2 0.35 0.1, V2 0.35 0.6, V2 0.25 0.6, V2 0.15 0.2, V2 0.25 0.1], FromLeft)
    , ( path [V2 0.65 0.1, V2 0.65 0.6, V2 0.75 0.6, V2 0.85 0.2, V2 0.75 0.1], FromLeft)
    , ( ngonPath 8 (V2 0.5 0.3) 0.25 0.0, FromTop )
    ]

pyramid :: [(Path, Shade)]
pyramid =
    [ ( path [V2 0.1 0.1, V2 0.45 0.45, V2 0.1 0.9], FromLeft )
    , ( path [V2 0.1 0.1, V2 0.45 0.45, V2 0.9 0.1], FromBottom )
    , ( path [V2 0.9 0.1, V2 0.45 0.45, V2 0.1 0.9], FromBlack )
    ]

scarab :: [(Path, Shade)]
scarab =
    [ ( ngonPath 5 (V2 0.3 0.7) 0.15 0.0, FromLeft )
    , ( ngonPath 8 (V2 0.5 0.5) 0.20 0.0, FromRight )
    , ( ngonPath 5 (V2 0.6 0.3) 0.15 0.0, FromTop )
    , ( ngonPath 5 (V2 0.7 0.4) 0.15 0.3, FromTop)
    ]

drawPiece :: Palette -> [(Path, Shade)] -> [(Path, Shade)] -> Orientation -> Form e
drawPiece pal base role o =
    group [ draw (rotatePiece o base) (pal Map.!)
          , move (V2 0.05 (- 0.05)) $ draw (rotatePiece o role) (const shadowColor)
          , draw (rotatePiece o role) (pal Map.!) ]
    where draw ps c = group [filled (c s) (polygon p) | (p, s) <- ps]
