module Main where

import Criterion.Main

import qualified BearingBG
import qualified GreatCircleBG
import qualified KinematicsBG
import qualified PositionBG

main :: IO ()
main =
    defaultMain
        [BearingBG.benchmark, GreatCircleBG.benchmark, KinematicsBG.benchmark, PositionBG.benchmark]