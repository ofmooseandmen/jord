module Main where

import Criterion.Main

import qualified GeodesicBG
import qualified GeodeticBG
import qualified GreatCircleBG
import qualified KinematicsBG
import qualified PositionsBG

main :: IO ()
main =
    defaultMain
        [ GeodesicBG.benchmark
        , GeodeticBG.benchmark
        , GreatCircleBG.benchmark
        , KinematicsBG.benchmark
        , PositionsBG.benchmark
        ]
