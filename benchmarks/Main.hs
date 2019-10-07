module Main where

import Criterion.Main

import qualified GeodesicBG
import qualified GreatCircleBG
import qualified KinematicsBG
import qualified PositionBG

main :: IO ()
main = defaultMain [GeodesicBG.benchmark, GreatCircleBG.benchmark, KinematicsBG.benchmark, PositionBG.benchmark]