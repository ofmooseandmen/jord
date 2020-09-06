module GeodesicBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle (decimalDegrees)
import qualified Data.Geo.Jord.Geodesic as Geodesic
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres)
import Data.Geo.Jord.Models (WGS84(..))

benchmark :: Benchmark
benchmark =
    bgroup
        "Geodesic"
        [ bench "direct" $ whnf (Geodesic.direct p1 b1) d1
        , bench "inverse" $ whnf (Geodesic.inverse p1) p2
        , bench "inverse antipodal" $ whnf (Geodesic.inverse p1) (Geodetic.antipode p1)
        , bench "inverse near-antipodal" $ whnf (Geodesic.inverse p3) p4
        ]

p1 :: HorizontalPosition WGS84
p1 = Geodetic.latLongPos (-37.95103341666667) 144.42486788888888 WGS84

p2 :: HorizontalPosition WGS84
p2 = Geodetic.latLongPos (-37.65282113888889) 143.92649552777777 WGS84

d1 :: Length
d1 = Length.metres 54972.271139

b1 :: Angle
b1 = Angle.decimalDegrees 306.86815920333333

p3 :: HorizontalPosition WGS84
p3 = Geodetic.latLongPos 0 0 WGS84

p4 :: HorizontalPosition WGS84
p4 = Geodetic.latLongPos 0.5 179.5 WGS84
