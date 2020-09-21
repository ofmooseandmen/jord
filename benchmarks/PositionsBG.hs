module PositionsBG
    ( benchmark
    ) where

import Criterion.Types
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres)
import Data.Geo.Jord.Models (S84(..), WGS84(..))
import qualified Data.Geo.Jord.Positions as Positions

benchmark :: Benchmark
benchmark =
    bgroup
        "Positions"
        [ bgroup
              "ellipsoidal"
              [ bench "toGeodetic" $ whnf Positions.toGeodetic egeoc
              , bench "toGeocentric" $ whnf Positions.toGeocentric egeod
              ]
        , bgroup
              "spherical"
              [ bench "toGeodetic" $ whnf Positions.toGeodetic sgeoc
              , bench "toGeocentric" $ whnf Positions.toGeocentric sgeod
              ]
        ]

egeoc :: Geocentric.Position WGS84
egeoc = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84

egeod :: Geodetic.Position WGS84
egeod = Geodetic.latLongHeightPos 45 45 h WGS84

sgeoc :: Geocentric.Position S84
sgeoc = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 S84

sgeod :: Geodetic.Position S84
sgeod = Geodetic.latLongHeightPos 45 45 h S84

h :: Length
h = Length.metres 15000
