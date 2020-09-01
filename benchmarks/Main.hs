module Main where

import Criterion.Main

import qualified PositionBG
import qualified GeodesicBG
import qualified GeodeticBG
import qualified GreatCircleBG
import qualified KinematicsBG

main :: IO ()
main =
    defaultMain
        [ PositionBG.benchmark
        , GeodesicBG.benchmark
        , GreatCircleBG.benchmark
        , KinematicsBG.benchmark
        , GeodeticBG.benchmark
        ]
