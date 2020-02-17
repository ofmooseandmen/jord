module GeodesicBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Geodesic
import Data.Geo.Jord.Position

benchmark :: Benchmark
benchmark =
    bgroup
        "Geodesic"
        [ bench "direct" $ whnf (directGeodesic p1 b1) d1
        , bench "inverse" $ whnf (inverseGeodesic p1) p2
        , bench "inverse antipodal" $ whnf (inverseGeodesic p1) (antipode p1)
        , bench "inverse near-antipodal" $ whnf (inverseGeodesic p3) p4
        ]

p1 :: Position WGS84
p1 = latLongPos (-37.95103341666667) 144.42486788888888 WGS84

p2 :: Position WGS84
p2 = latLongPos (-37.65282113888889) 143.92649552777777 WGS84

d1 :: Length
d1 = metres 54972.271139

b1 :: Angle
b1 = decimalDegrees 306.86815920333333

p3 :: Position WGS84
p3 = latLongPos 0 0 WGS84

p4 :: Position WGS84
p4 = latLongPos 0.5 179.5 WGS84