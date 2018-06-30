-- |
-- TODO add doc
--
module Data.Geo.Jord.NVector
    ( NVector(x, y, z)
    , nvector
    , add
    , cross
    , dot
    , norm
    , normalise
    , scale
    , subtract
    , zero
    ) where

import Prelude hiding (subtract)

data NVector = NVector
    { x :: Double
    , y :: Double
    , z :: Double
    } deriving (Eq, Show)

nvector :: Double -> Double -> Double -> NVector
nvector x' y' z' = normalise (NVector x' y' z')

add :: NVector -> NVector -> NVector
add a b = NVector x' y' z'
  where
    x' = x a + x b
    y' = y a + y b
    z' = z a + z b

cross :: NVector -> NVector -> NVector
cross a b = NVector x' y' z'
  where
    x' = y a * z b - z a * y b
    y' = z a * x b - x a * z b
    z' = x a * y b - y a * x b

dot :: NVector -> NVector -> Double
dot a b = x a * x b + y a * y b + z a * z b

norm :: NVector -> Double
norm a = sqrt ((x a * x a) + (y a * y a) + (z a * z a))

normalise :: NVector -> NVector
normalise a = scale a s
  where
    s = 1.0 / norm a

scale :: NVector -> Double -> NVector
scale a s = NVector x' y' z'
  where
    x' = x a * s
    y' = y a * s
    z' = z a * s

subtract :: NVector -> NVector -> NVector
subtract a b = NVector x' y' z'
  where
    x' = x a - x b
    y' = y a - y b
    z' = z a - z b

zero :: NVector
zero = NVector 0.0 0.0 0.0
