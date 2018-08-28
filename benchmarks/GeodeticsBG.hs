module GeodeticsBG
    ( bggeodetics
    ) where

import Criterion.Types
import Data.Geo.Jord

bggeodetics :: Benchmark
bggeodetics =
    bgroup
        "Geodetics"
        [ bench "angularDistance" $ whnf (angularDistance nv1 nv2) (Just nv3)
        , bench "crossTrackDistance" $ whnf (crossTrackDistance84 nv3) gc
        , bench "destination" $ whnf (destination84 nv1 a) l
        , bench "finalBearing" $ whnf (finalBearing nv1) nv2
        , bench "initialBearing" $ whnf (initialBearing nv1) nv2
        , bench "interpolate" $ whnf (interpolate nv1 nv2) 0.5
        ]

nv1 :: NVector
nv1 = nvector 0.5504083453140064 0.12711022980808237 0.8251627978083076

nv2 :: NVector
nv2 = nvector 0.484947835927087 0.1582112780286092 0.860113241343365

nv3 :: NVector
nv3 = nvector 0.5225962210695282 0.11083913756305296 0.8453448262739457

gc :: GreatCircle
gc = greatCircle (nv1, nv2)

a :: Angle
a = decimalDegrees 45.0

l :: Length
l = kilometres 5000
