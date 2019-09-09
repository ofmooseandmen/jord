module BearingBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord

benchmark :: Benchmark
benchmark =
    bgroup
        "Bearing"
        [ bench "finalBearing (spherical)" $ whnf (finalBearing s1) s2
        , bench "initialBearing (spherical)" $ whnf (initialBearing s1) s2
        ]

s1 :: Position S84
s1 = nvectorPos 0.5504083453140064 0.12711022980808237 0.8251627978083076 S84

s2 :: Position S84
s2 = nvectorPos 0.484947835927087 0.1582112780286092 0.860113241343365 S84