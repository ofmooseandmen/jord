module Main where

import Criterion.Main

import qualified ConversionBG
import qualified GeodesicBG
import qualified GeodeticBG
import qualified GreatCircleBG
import qualified KinematicsBG

main :: IO ()
main =
    defaultMain
        [ ConversionBG.benchmark
        , GeodesicBG.benchmark
        , GreatCircleBG.benchmark
        , KinematicsBG.benchmark
        , GeodeticBG.benchmark
        ]
