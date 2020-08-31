module ConversionBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Conversion
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres)
import Data.Geo.Jord.Models (S84(..), WGS84(..))

benchmark :: Benchmark
benchmark =
    bgroup
        "Position"
        [ bench "toGeodetic (ellipsoidal)" $ whnf toGeodetic egeoc
        , bench "toGeocentric (ellipsoidal)" $ whnf toGeocentric egeod
        , bench "toGeodetic (spherical)" $ whnf toGeodetic sgeoc
        , bench "toGeocentric (spherical)" $ whnf toGeocentric sgeod
        ]

egeoc :: Geocentric.Position WGS84
egeoc = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84

egeod :: Geodetic.Position WGS84
egeod = Geodetic.wgs84Pos 45 45 h

sgeoc :: Geocentric.Position S84
sgeoc = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 S84

sgeod :: Geodetic.Position S84
sgeod = Geodetic.s84Pos 45 45 h

h :: Length
h = Length.metres 15000
