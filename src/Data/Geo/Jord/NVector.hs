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
    , cross
    , dot
    , norm
    , scale
    , unit
    , zero
    ) where

import Data.Geo.Jord.Quantity

-- | Represents a position as the normal vector to the Earth model.
data NVector = NVector
    { x :: Double
    , y :: Double
    , z :: Double
    }

-- | Add and subtract 'NVector's.
instance Quantity NVector where
    add a b = NVector x' y' z'
      where
        x' = x a + x b
        y' = y a + y b
        z' = z a + z b
    sub a b = NVector x' y' z'
      where
        x' = x a - x b
        y' = y a - y b
        z' = z a - z b

-- | Smart 'NVector' constructor. The returned 'NVector' is a 'unit' vector.
nvector :: Double -> Double -> Double -> NVector
nvector x' y' z' = unit (NVector x' y' z')

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

-- | Multiplies each component of the given 'NVector' by the given value.
scale :: NVector -> Double -> NVector
scale a s = NVector x' y' z'
  where
    x' = x a * s
    y' = y a * s
    z' = z a * s

-- | Normalises the given 'NVector'.
unit :: NVector -> NVector
unit a = scale a s
  where
    s = 1.0 / norm a

-- | [0, 0, 0] - not a valid 'NVector', but can be used as the identity value during reduction.
zero :: NVector
zero = NVector 0.0 0.0 0.0
