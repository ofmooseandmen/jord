module TransformationBG
    ( bgtransformation
    ) where

import Criterion.Types
import Data.Geo.Jord

bgtransformation :: Benchmark
bgtransformation =
    bgroup
        "Transformation"
        [ bench "latLongToNVector" $ whnf latLongToNVector ll
        , bench "nvectorToLatLong" $ whnf nvectorToLatLong nv
        , bgroup
              "Ellipsoidal"
              [ bench "ecefToNVector" $ whnf (`ecefToNVector` wgs84) ep
              , bench "nvectorToEcef" $ whnf (`nvectorToEcef` wgs84) ap
              ]
        , bgroup
              "Spherical"
              [ bench "ecefToNVector" $ whnf (`ecefToNVector` s84) ep
              , bench "nvectorToEcef" $ whnf (`nvectorToEcef` s84) ap
              ]
        ]

ll :: LatLong
ll = decimalLatLong 55.6050 13.0038

ap :: AngularPosition NVector
ap = AngularPosition nv (metres 15000.0)

nv :: NVector
nv = nvector 0.5 0.5 0.7071

ep :: EcefPosition
ep = ecefMetres 5733855.7748 (-6370998.3802) 7008137.5108
