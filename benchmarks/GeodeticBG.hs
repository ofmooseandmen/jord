module GeodeticBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle (decimalDegrees)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Math3d (V3)

benchmark :: Benchmark
benchmark =
    bgroup
        "Geodetic"
        [ bench "nvectorFromLatLong" $ whnf Geodetic.nvectorFromLatLong ll
        , bench "nvectorToLatLong" $ whnf Geodetic.nvectorToLatLong nv
        ]

ll :: (Angle, Angle)
ll = (Angle.decimalDegrees 55.6050, Angle.decimalDegrees 13.0038)

nv :: V3
nv = Geodetic.nvectorFromLatLong ll
