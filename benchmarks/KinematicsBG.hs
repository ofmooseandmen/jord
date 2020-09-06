module KinematicsBG
    ( benchmark
    ) where

import Criterion.Types
import qualified Data.Geo.Jord.Angle as Angle (decimalDegrees)
import qualified Data.Geo.Jord.Duration as Duration (seconds)
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Kinematics (Track(..))
import qualified Data.Geo.Jord.Kinematics as Kinematics
import Data.Geo.Jord.Models (S84(..))
import qualified Data.Geo.Jord.Speed as Speed (knots)

benchmark :: Benchmark
benchmark =
    bgroup
        "Kinematics"
        [ bgroup
              "CPA"
              [ bench "in the past" $ whnf (Kinematics.cpa t1) t2
              , bench "in the future" $ whnf (Kinematics.cpa t3) t4
              , bench "same positions" $ whnf (Kinematics.cpa t1') t1
              ]
        , bgroup
              "intercept"
              [ bench "min speed" $ whnf (Kinematics.intercept t5) ip1
              , bench "by speed" $ whnf (Kinematics.interceptBySpeed t5 ip1) (Speed.knots 700)
              , bench "by time" $ whnf (Kinematics.interceptByTime t5 ip1) (Duration.seconds 2700)
              ]
        ]

t1 :: Track S84
t1 = Track (Geodetic.s84Pos 20 (-60)) (Angle.decimalDegrees 10) (Speed.knots 15)

t1' :: Track S84
t1' = Track (Geodetic.s84Pos 20 (-60)) (Angle.decimalDegrees 10) (Speed.knots 15)

t2 :: Track S84
t2 = Track (Geodetic.s84Pos 34 (-50)) (Angle.decimalDegrees 220) (Speed.knots 300)

t3 :: Track S84
t3 = Track (Geodetic.s84Pos 30 30) (Angle.decimalDegrees 45) (Speed.knots 400)

t4 :: Track S84
t4 = Track (Geodetic.s84Pos 30.01 30) (Angle.decimalDegrees 315) (Speed.knots 400)

t5 :: Track S84
t5 = Track (Geodetic.s84Pos 34 (-50)) (Angle.decimalDegrees 220) (Speed.knots 600)

ip1 :: HorizontalPosition S84
ip1 = Geodetic.s84Pos 20 (-60)
