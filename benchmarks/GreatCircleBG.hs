module GreatCircleBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord

benchmark :: Benchmark
benchmark =
    bgroup
        "Great Circle"
        [ bench "alongTrackDistance" $ whnf (alongTrackDistance' p1 p2) a
        , bench "angularDistance" $ whnf (angularDistance p1 p2) (Just p3)
        , bench "crossTrackDistance" $ whnf (crossTrackDistance' p1 p2) a
        , bench "destinationS" $ whnf (destinationS p1 a) l
        , bench "interpolate" $ whnf (interpolate p1 p2) 0.5
        , bench "surfaceDistanceS" $ whnf (surfaceDistanceS p1) p2
        , bench "finalBearingS" $ whnf (finalBearingS p1) p2
        , bench "initialBearingS" $ whnf (initialBearingS p1) p2
        ]

p1 :: Position S84
p1 = nvectorPos 0.5504083453140064 0.12711022980808237 0.8251627978083076 S84

p2 :: Position S84
p2 = nvectorPos 0.484947835927087 0.1582112780286092 0.860113241343365 S84

p3 :: Position S84
p3 = nvectorPos 0.5225962210695282 0.11083913756305296 0.8453448262739457 S84

a :: Angle
a = decimalDegrees 45.0

l :: Length
l = kilometres 5000
