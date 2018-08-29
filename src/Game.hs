module Game
    (
    ) where

import qualified Data.Map as Map

data Orientation = North
                 | West
                 | South
                 | East
                 deriving (Enum, Show)

turnLeft :: Orientation -> Orientation
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Orientation -> Orientation
turnRight North = East
turnRight West  = North
turnRight South = West
turnRight East  = South

flip :: Orientation -> Orientation
flip South = North
flip North = South
flip East  = West
flip West  = East

data Piece = Pharaoh
           | Anubis
           | Pyramid
           | Scarab
           | Sphinx
           deriving (Enum, Show)

data Colour = Red
            | White
            | Neutral
            deriving (Show, Eq)

data Asset = Asset
    { piece       :: Piece
    , orientation :: Orientation
    , colour      :: Colour
    }

type Position = (Int, Int)

type Board = Map.Map Position (Maybe Asset)

data Ray = Ray Position Orientation

data Consequence = Kill Position
                 | Lose Colour
                 | Stop

-- Pyramid: North |\ , West /|, South \|, East |/
-- mapping pyramid and ray orientation
pyramidReflectionMap :: Map.Map (Orientation, Orientation) Orientation = Map.fromList
    [((North, South), East),
     ((North, West),  North),
     ((West,  East),  North),
     ((West,  South), West),
     ((South, East),  South),
     ((South, North), West),
     ((East,  West),  South),
     ((East,  North), East)] 

propagateAsset :: Ray -> Asset -> Either Consequence Ray
propagateAsset _          (Asset Pharaoh _  c) = Left $ Lose c

propagateAsset (Ray p or) (Asset Anubis  oa _)
    | or == oa  = Left Stop
    | otherwise = Left $ Kill p

propagateAsset (Ray p or) (Asset Pyramid oa _) =
    case Map.lookup (oa, or) of
        Just o  -> Right $ forward $ Ray p o
        Nothing -> Left $ Kill p

propagateAsset (Ray p or) (Asset Scarab oa _)
    | or == oa || or == flip oa = Right $ forward $ Ray p $ turnLeft or
    | otherwise                 = Right $ forward $ Ray p $ turnRight or

propagateAsset r          (Asset Sphinx _  _) = Left Stop

propagate :: Ray -> Maybe Asset -> Either Consequence Ray
propagate r Nothing  = Right $ forward r
propagate r (Just a) = propagateAsset r a

fireRay :: Board -> Ray -> Consequence
fireRay board ray@(Ray p _) = 
    let nextRay = propagate ray (Map.lookup p board)
    in case nextRay of
        Left c  -> c
        Right r -> fireRay board r

