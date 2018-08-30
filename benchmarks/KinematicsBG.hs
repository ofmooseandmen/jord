module KinematicsBG
    ( bgkinematics
    ) where

import Criterion.Types
import Data.Geo.Jord

bgkinematics :: Benchmark
bgkinematics =
    bgroup
        "Kinematics"
        [ bgroup
              "CPA"
              [ bench "in the past" $ whnf (cpa84 t1) t2
              , bench "in the future" $ whnf (cpa84 t3) t4
              , bench "same positions" $ whnf (cpa84 t1') t1
              ]
        , bgroup
              "intercept"
              [ bench "min speed" $ whnf (intercept84 t5) ip1
              , bench "by speed" $ whnf (interceptBySpeed84 t5 ip1) (knots 700)
              , bench "by time" $ whnf (interceptByTime84 t5 ip1) (seconds 2700)
              ]
        ]

t1 :: Track NVector
t1 = Track (latLongToNVector (decimalLatLong 20 (-60))) (decimalDegrees 10) (knots 15)

t1' :: Track NVector
t1' = Track (latLongToNVector (decimalLatLong 20 (-60))) (decimalDegrees 10) (knots 15)

t2 :: Track NVector
t2 = Track (latLongToNVector (decimalLatLong 34 (-50))) (decimalDegrees 220) (knots 300)

t3 :: Track NVector
t3 = Track (latLongToNVector (decimalLatLong 30 30)) (decimalDegrees 45) (knots 400)

t4 :: Track NVector
t4 = Track (latLongToNVector (decimalLatLong 30.01 30)) (decimalDegrees 315) (knots 400)

t5 :: Track NVector
t5 = Track (latLongToNVector (decimalLatLong 34 (-50))) (decimalDegrees 220) (knots 600)

ip1 :: NVector
ip1 = latLongToNVector (decimalLatLong 20 (-60))
