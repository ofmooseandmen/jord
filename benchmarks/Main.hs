module Main where

import Criterion.Main

import qualified GreatCircleBG
import qualified KinematicsBG
import qualified PositionBG

main :: IO ()
main = defaultMain [GreatCircleBG.benchmark, KinematicsBG.benchmark, PositionBG.benchmark]
