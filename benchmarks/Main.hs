module Main where

import Criterion.Main
import GeodeticsBG
import KinematicsBG
import TransformationBG

main :: IO ()
main = defaultMain [bggeodetics, bgkinematics, bgtransformation]
