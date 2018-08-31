module PaintBoard where

import Game
import Graphics.Gloss
import qualified Data.Map.Strict as Map

paintSquare :: Picture
paintSquare = color blue $ rectangleSolid 0.9 0.9

colourChar :: Colour -> String
colourChar White = "w"
colourChar Red = "r"

paintPiece :: Piece -> Colour -> IO Picture
paintPiece Pharaoh c = scale 0.016 0.016 
    <$> loadBMP ("./data/pharaoh-" ++ colourChar c ++ ".bmp")
paintPiece Anubis c = scale 0.016 0.016
    <$> loadBMP ("./data/anubis-" ++ colourChar c ++ ".bmp")
paintPiece Pyramid c = scale 0.016 0.016
    <$> loadBMP ("./data/pyramid-" ++ colourChar c ++ ".bmp")
paintPiece Scarab c = scale 0.016 0.016
    <$> loadBMP ("./data/scarab-" ++ colourChar c ++ ".bmp")
paintPiece Sphinx c = scale 0.016 0.016
    <$> loadBMP ("./data/sphinx-" ++ colourChar c ++ ".bmp")

paintBackground :: Picture
paintBackground = 
    pictures [translate (fromIntegral x) (fromIntegral y) paintSquare
             | x <- [0..9], y <- [0..7]]

paintAsset :: (Position, Asset) -> IO Picture
paintAsset ((x, y), Asset piece o c) = do
    p <- rotate ((- 90.0) * fromIntegral (fromEnum o)) <$> paintPiece piece c
    return $ translate (fromIntegral x) (fromIntegral y) p

paintAssets :: Board -> IO Picture
paintAssets board = do
    ps <- mapM paintAsset $ Map.toList board
    return $ pictures ps

paintBoard :: Board -> IO Picture
paintBoard board = do
    pieces <- paintAssets board
    return $ pictures [paintBackground, pieces]

paintPath :: LaserPath -> Picture
paintPath p = line $ map (\(x, y) -> (fromIntegral x, fromIntegral y)) p