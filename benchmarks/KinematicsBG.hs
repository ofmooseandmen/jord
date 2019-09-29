module KinematicsBG
    ( benchmark
    ) where

import Criterion.Types
import Data.Geo.Jord.Position
import Data.Geo.Jord.Kinematics

benchmark :: Benchmark
benchmark =
    bgroup
        "Kinematics"
        [ bgroup
              "CPA"
              [ bench "in the past" $ whnf (cpa t1) t2
              , bench "in the future" $ whnf (cpa t3) t4
              , bench "same positions" $ whnf (cpa t1') t1
              ]
        , bgroup
              "intercept"
              [ bench "min speed" $ whnf (intercept t5) ip1
              , bench "by speed" $ whnf (interceptBySpeed t5 ip1) (knots 700)
              , bench "by time" $ whnf (interceptByTime t5 ip1) (seconds 2700)
              ]
        ]

t1 :: Track S84
t1 = Track (s84Pos 20 (-60) zero) (decimalDegrees 10) (knots 15)

t1' :: Track S84
t1' = Track (s84Pos 20 (-60) zero) (decimalDegrees 10) (knots 15)

t2 :: Track S84
t2 = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 300)

t3 :: Track S84
t3 = Track (s84Pos 30 30 zero) (decimalDegrees 45) (knots 400)

t4 :: Track S84
t4 = Track (s84Pos 30.01 30 zero) (decimalDegrees 315) (knots 400)

t5 :: Track S84
t5 = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 600)

ip1 :: Position S84
ip1 = s84Pos 20 (-60) zero