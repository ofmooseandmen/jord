module ConversionBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle (decimalDegrees)
import Data.Geo.Jord.Conversion
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Ellipsoids (eWGS84)
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres)
import Data.Geo.Jord.Math3d (V3)

benchmark :: Benchmark
benchmark =
    bgroup
        "Position"
        [ bench "nvectorFromGeocentric (ellipsoidal)" $ whnf (`nvectorFromGeocentric` e) gce
        , bench "nvectorToGeocentric (ellipsoidal)" $ whnf (`nvectorToGeocentric` e) (nv, h)
        , bench "nvectorFromGeocentric (spherical)" $ whnf (`nvectorFromGeocentric` s) gcs
        , bench "nvectorToGeocentric (spherical)" $ whnf (`nvectorToGeocentric` s) (nv, h)
        ]

ll :: (Angle, Angle)
ll = (Angle.decimalDegrees 55.6050, Angle.decimalDegrees 13.0038)

nv :: V3
nv = Geodetic.nvectorFromLatLong ll

gce :: Geocentric.Coordinates
gce = nvectorToGeocentric (nv, h) s

gcs :: Geocentric.Coordinates
gcs = nvectorToGeocentric (nv, h) e

h :: Length
h = Length.metres 15000

s :: Ellipsoid
s = toSphere eWGS84

e :: Ellipsoid
e = eWGS84
