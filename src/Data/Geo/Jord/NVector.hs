-- |
-- Module:      Data.Geo.Jord.NVector
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with n-vectors.
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

-- | Represents a position as the normal vector to the Earth model.
data NVector = NVector
    { x :: Double
    , y :: Double
    , z :: Double
    } deriving (Eq, Show)

-- | Smart 'NVector' constructor. The returned 'NVector' is normalised.
nvector :: Double -> Double -> Double -> NVector
nvector x' y' z' = normalise (NVector x' y' z')

-- | Adds the two given 'NVector's.
add :: NVector -> NVector -> NVector
add a b = NVector x' y' z'
  where
    x' = x a + x b
    y' = y a + y b
    z' = z a + z b

-- | Computes the cross product of the two given 'NVector's.
cross :: NVector -> NVector -> NVector
cross a b = NVector x' y' z'
  where
    x' = y a * z b - z a * y b
    y' = z a * x b - x a * z b
    z' = x a * y b - y a * x b

-- | Computes the dot product of the two given 'NVector's.
dot :: NVector -> NVector -> Double
dot a b = x a * x b + y a * y b + z a * z b

-- | Computes the norm of the given 'NVector'.
norm :: NVector -> Double
norm a = sqrt ((x a * x a) + (y a * y a) + (z a * z a))

-- | Normalises the given 'NVector'.
normalise :: NVector -> NVector
normalise a = scale a s
  where
    s = 1.0 / norm a

-- | Multiplies each component of the given 'NVector' by the given value.
scale :: NVector -> Double -> NVector
scale a s = NVector x' y' z'
  where
    x' = x a * s
    y' = y a * s
    z' = z a * s

-- | Subtracts the two given 'NVector's.
subtract :: NVector -> NVector -> NVector
subtract a b = NVector x' y' z'
  where
    x' = x a - x b
    y' = y a - y b
    z' = z a - z b

zero :: NVector
zero = NVector 0.0 0.0 0.0
