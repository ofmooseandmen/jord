module GreatCircleBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle (decimalDegrees)
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (kilometres)
import Data.Geo.Jord.Models (S84(..))

benchmark :: Benchmark
benchmark =
    bgroup
        "Great Circle"
        [ bench "alongTrackDistance" $ whnf (GreatCircle.alongTrackDistance' p1 p2) a
        , bench "angularDistance" $ whnf (GreatCircle.angularDistance p1 p2) (Just p3)
        , bench "crossTrackDistance" $ whnf (GreatCircle.crossTrackDistance' p1 p2) a
        , bench "destination" $ whnf (GreatCircle.destination p1 a) l
        , bench "interpolated" $ whnf (GreatCircle.interpolated p1 p2) 0.5
        , bench "distance" $ whnf (GreatCircle.distance p1) p2
        , bench "finalBearing" $ whnf (GreatCircle.finalBearing p1) p2
        , bench "initialBearing" $ whnf (GreatCircle.initialBearing p1) p2
        ]

p1 :: HorizontalPosition S84
p1 = Geodetic.nvectorPos 0.5504083453140064 0.12711022980808237 0.8251627978083076 S84

p2 :: HorizontalPosition S84
p2 = Geodetic.nvectorPos 0.484947835927087 0.1582112780286092 0.860113241343365 S84

p3 :: HorizontalPosition S84
p3 = Geodetic.nvectorPos 0.5225962210695282 0.11083913756305296 0.8453448262739457 S84

a :: Angle
a = Angle.decimalDegrees 45.0

l :: Length
l = Length.kilometres 5000
