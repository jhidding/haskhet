module Game where

import qualified Data.Map.Strict as Map
import Control.Monad.State

data Orientation = North
                 | West
                 | South
                 | East
                 deriving (Enum, Eq, Ord, Show)

data Twist = TwistLeft | TwistRight

orientationToDirection :: Map.Map Orientation -> Position
orientationToDirection o = Map.lookup o $ Map.fromList
    [ (North, ( 0,  1))
    , (West,  (-1,  0))
    , (South, ( 0, -1))
    , (East,  ( 1,  0)) ]

translate :: Position -> Orientation -> Position
translate (x, y) o = (x + dx, y + dy)
    where (dx, dy) = orientationToDirection o

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

turnFlip :: Orientation -> Orientation
turnFlip South = North
turnFlip North = South
turnFlip East  = West
turnFlip West  = East

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

data PlayerTurn = Move   Position Orientation
                | Rotate Position Twist

data Asset = Asset
    { piece       :: Piece
    , orientation :: Orientation
    , colour      :: Colour
    }

type Position = (Int, Int)

type Board = Map.Map Position Asset

data Ray = Ray Position Orientation

forward :: Ray -> Ray
forward (Ray (x, y) North) = Ray (x, y + 1) North
forward (Ray (x, y) West)  = Ray (x - 1, y) West
forward (Ray (x, y) South) = Ray (x, y - 1) South
forward (Ray (x, y) East)  = Ray (x + 1, y) East

data Consequence = Kill Position
                 | Lose Colour
                 | Stop

-- Pyramid: North |\ , West /|, South \|, East |/
-- mapping pyramid and ray orientation
pyramidReflectionMap :: Map.Map (Orientation, Orientation) Orientation 
pyramidReflectionMap = Map.fromList
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
    | or == turnFlip oa  = Left Stop
    | otherwise          = Left $ Kill p

propagateAsset (Ray p or) (Asset Pyramid oa _) =
    case Map.lookup (oa, or) pyramidReflectionMap of
        Just o  -> Right $ forward $ Ray p o
        Nothing -> Left $ Kill p

propagateAsset (Ray p or) (Asset Scarab oa _)
    | or == oa || or == turnFlip oa = Right $ forward $ Ray p $ turnLeft or
    | otherwise                     = Right $ forward $ Ray p $ turnRight or

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

type BoardState a = State Board a

moveAsset :: Position -> Position -> BoardState (Maybe Asset)
moveAsset p1 p2 = do
    asset <- Map.lookup p1 <$> get
    case asset of
        Just asset -> modify (Map.insert p2 asset . Map.delete p1)
        Nothing    -> return ()
    return asset

data MoveType = SimpleMove | SwapMove | NoMove

moveType :: Asset -> Maybe Asset -> MoveType
moveType (Asset Sphinx _ _) _                          = NoMove
moveType _                  Nothing                    = SimpleMove
moveType (Asset Scarab _ _) (Just (Asset Pyramid _ _)) = SwapMove
moveType (Asset Scarab _ _) (Just (Asset Anubis  _ _)) = SwapMove
moveType _                  _                          = NoMove

tryMove :: Asset -> Position -> Orientation -> BoardState Bool
tryMove a p1 o
    | mt == SwapMove   = swapAssets p1 p2 >> return True
    | mt == SimpleMove = moveAsset p1 p2  >> return True
    | otherwise        = return False
    where mt = moveType a $ gets $ Map.lookup p2
          p2 = translate p1 o

updateBoard :: PlayerTurn -> BoardState Bool
updateBoard (Move p o) = do
    asset <- gets $ Map.lookup p
    case asset of
        Nothing -> return False
        Just a  -> tryMove a p o
    
-- Pyramid: North |\ , West /|, South \|, East |/
-- Scarab : North *\, South \, West /, East /* (* is the head of the scarab)
classicBoard :: Board
classicBoard = Map.fromList
    [((0, 2), Asset Pyramid West  White),
     ((0, 3), Asset Anubis  North White),
     ((0, 4), Asset Pharaoh North White),
     ((0, 5), Asset Anubis  North White),
     ((0, 9), Asset Sphinx  North White),
     ((1, 7), Asset Pyramid North White),
     ((2, 6), Asset Pyramid West  Red),
     ((3, 0), Asset Pyramid West  Red),
     ((3, 2), Asset Pyramid West  White),
     ((3, 4), Asset Scarab  West  White),
     ((3, 5), Asset Scarab  South White),
     ((3, 7), Asset Pyramid North Red),
     ((3, 9), Asset Pyramid South White),
     ((4, 0), Asset Pyramid South Red),
     ((4, 2), Asset Pyramid West  White),
     ((4, 4), Asset Scarab  North Red),
     ((4, 5), Asset Scarab  East  Red),
     ((4, 7), Asset Pyramid East  Red),
     ((4, 9), Asset Pyramid West  White),
     ((5, 3), Asset Pyramid South Red),
     ((6, 2), Asset Pyramid South Red),
     ((7, 0), Asset Sphinx  South Red),
     ((7, 4), Asset Anubis  South Red),
     ((7, 5), Asset Pharaoh South Red),
     ((7, 6), Asset Anubis  South Red),
     ((7, 7), Asset Pyramid East  Red)]