module PositionBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord

benchmark :: Benchmark
benchmark =
    bgroup
        "Position"
        [ bench "nvectorFromLatLong" $ whnf nvectorFromLatLong ll
        , bench "nvectorToLatLong" $ whnf nvectorToLatLong nv
        , bench "nvectorFromEcef (ellipsoidal)" $ whnf (`nvectorFromEcef` e) eve
        , bench "nvectorToEcef (ellipsoidal)" $ whnf (`nvectorToEcef` e) (nv, h)
        , bench "nvectorFromEcef (ellipsoidal)" $ whnf (`nvectorFromEcef` s) evs
        , bench "nvectorToEcef (ellipsoidal)" $ whnf (`nvectorToEcef` s) (nv, h)
        ]

ll :: (Angle, Angle)
ll = (decimalDegrees 55.6050, decimalDegrees 13.0038)

nv :: NVector
nv = nvectorFromLatLong ll

eve :: EcefVector
eve = ecef (ecefMetresPos 5733855.7748 (-6370998.3802) 7008137.5108 WGS84)

evs :: EcefVector
evs = ecef (ecefMetresPos 5733855.7748 (-6370998.3802) 7008137.5108 S84)

h :: Length
h = metres 15000

s :: Shape
s = shape S84

e :: Shape
e = shape WGS84