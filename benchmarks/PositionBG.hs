module PositionBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Position

benchmark :: Benchmark
benchmark =
    bgroup
        "Position"
        [ bench "nvectorFromLatLong" $ whnf nvectorFromLatLong ll
        , bench "nvectorToLatLong" $ whnf nvectorToLatLong nv
        , bench "nvectorFromGeocentric (ellipsoidal)" $ whnf (`nvectorFromGeocentric` e) gce
        , bench "nvectorToGeocentric (ellipsoidal)" $ whnf (`nvectorToGeocentric` e) (nv, h)
        , bench "nvectorFromGeocentric (spherical)" $ whnf (`nvectorFromGeocentric` s) gcs
        , bench "nvectorToGeocentric (spherical)" $ whnf (`nvectorToGeocentric` s) (nv, h)
        ]

ll :: (Angle, Angle)
ll = (decimalDegrees 55.6050, decimalDegrees 13.0038)

nv :: NVector
nv = nvectorFromLatLong ll

gce :: Geocentric
gce = geocentric (geocentricMetresPos 5733855.7748 (-6370998.3802) 7008137.5108 WGS84)

gcs :: Geocentric
gcs = geocentric (geocentricMetresPos 5733855.7748 (-6370998.3802) 7008137.5108 S84)

h :: Length
h = metres 15000

s :: Ellipsoid
s = toSphere eWGS84

e :: Ellipsoid
e = eWGS84