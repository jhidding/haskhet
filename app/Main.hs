module Main where

import Controller
import Model
import View

import Game

import Linear.V2 (V2(V2))
import qualified Helm
import qualified Helm.Cmd
import qualified Helm.Sub
import qualified Helm.Engine.SDL as SDL

import Helm.Graphics2D
import Helm.Color


initial :: (Model, Helm.Cmd SDL.SDLEngine Action)
initial = (initialState, Helm.Cmd.none)

main :: IO ()
main = do
    engine <- SDL.startupWith $ SDL.defaultConfig
        { SDL.windowIsResizable = False
        , SDL.windowDimensions = V2 1000 850
        }

    Helm.run engine Helm.GameConfig
        { Helm.initialFn       = initial
        , Helm.updateFn        = update
        , Helm.subscriptionsFn = subscriptions
        , Helm.viewFn          = view
        }