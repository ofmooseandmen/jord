module Main where

import Criterion.Main
import GeodeticsBG
import TransformationBG

main :: IO ()
main = defaultMain [bggeodetics, bgtransformation]
