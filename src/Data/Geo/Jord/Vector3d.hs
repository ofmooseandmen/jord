-- |
-- Module:      Data.Geo.Jord.Vector3d
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with 3 element vector.
--
module Data.Geo.Jord.Vector3d
    ( Vector3d(..)
    , cross
    , dot
    , norm
    , scale
    , unit
    ) where

import Data.Geo.Jord.Quantity

-- | A 3 element vector represented by double precision floating point x,y,z coordinates.
data Vector3d = Vector3d
    { x :: Double
    , y :: Double
    , z :: Double
    } deriving (Eq, Show)

-- | Add and subtract 'Vector3d's.
instance Quantity Vector3d where
    add a b = Vector3d x' y' z'
      where
        x' = x a + x b
        y' = y a + y b
        z' = z a + z b
    sub a b = Vector3d x' y' z'
      where
        x' = x a - x b
        y' = y a - y b
        z' = z a - z b
    zero = Vector3d 0 0 0

-- | Computes the cross product of the two given 'Vector3d's.
cross :: Vector3d -> Vector3d -> Vector3d
cross a b = Vector3d x' y' z'
  where
    x' = y a * z b - z a * y b
    y' = z a * x b - x a * z b
    z' = x a * y b - y a * x b

-- | Computes the dot product of the two given 'Vector3d's.
dot :: Vector3d -> Vector3d -> Double
dot a b = x a * x b + y a * y b + z a * z b

-- | Computes the norm of the given 'Vector3d'.
norm :: Vector3d -> Double
norm a = sqrt ((x a * x a) + (y a * y a) + (z a * z a))

-- | Multiplies each component of the given 'Vector3d' by the given value.
scale :: Vector3d -> Double -> Vector3d
scale a s = Vector3d x' y' z'
  where
    x' = x a * s
    y' = y a * s
    z' = z a * s

-- | Normalises the given 'Vector3d'.
unit :: Vector3d -> Vector3d
unit a = scale a s
  where
    s = 1.0 / norm a
