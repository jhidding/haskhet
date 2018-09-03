module View.Colors where

import qualified Data.Map.Strict as Map
import Helm.Color

backgroundColor = rgb 0.2 0.1 0.1
red = rgba 1.0 0.0 0.0 0.5
blue = rgba 0.0 0.0 1.0 0.5
white = rgb 1.0 1.0 1.0
tileColor1 = rgba 0.18 0.21 0.35 0.9
tileColor2 = rgba 0.15 0.15 0.25 0.9
selectedTileColor = rgb 0.5 0.6 0.8
shadowColor = rgba 0.0 0.0 0.0 0.5

data Shade =
    FromBottom | FromRight | FromTop | FromLeft | FromOver | FromBlack
    deriving (Show, Eq, Ord, Enum)

shades :: [Shade]
shades = enumFrom (toEnum 0)

type Palette = Map.Map Shade Color

redPalette :: Palette
redPalette = Map.fromList [
    (FromBottom, rgb 0.5 0.0 0.0),
    (FromRight,  rgb 0.4 0.1 0.1),
    (FromTop,    rgb 1.0 0.3 0.3),
    (FromLeft,   rgb 1.0 0.2 0.2),
    (FromOver,   rgb 0.6 0.1 0.1),
    (FromBlack,  rgb 0.0 0.0 0.0) ]

whitePalette :: Palette
whitePalette = Map.fromList [
    (FromBottom, rgb 0.4 0.4 0.0),
    (FromRight,  rgb 0.3 0.3 0.2),
    (FromTop,    rgb 0.8 0.8 0.1),
    (FromLeft,   rgb 0.9 0.9 0.3),
    (FromOver,   rgb 0.7 0.7 0.1),
    (FromBlack,  rgb 0.0 0.0 0.0) ]

tileGradient c1 c2 = radial (-0.2, 0.2) 0.0 (-0.2, 0.2) 1.4
                            [(0.0, c1), (1.0, c2)] 
