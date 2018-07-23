{-# LANGUAGE MultiParamTypeClasses #-}

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
    ( NVector(..)
    , cross
    , dot
    , rotate
    , scale
    , unit
    ) where

import Data.Geo.Jord.Quantity

-- | Represents a position as the normal vector to the sphere.
--
-- Orientation: z-axis points to the North Pole along the Earth's rotation axis,
-- x-axis points towards the point where latitude = longitude = 0.
data NVector = NVector
    { nx :: Double
    , ny :: Double
    , nz :: Double
    } deriving (Eq, Show)

-- | Add and subtract 'NVector's.
instance Quantity NVector where
    add a b = NVector x y z
      where
        x = nx a + nx b
        y = ny a + ny b
        z = nz a + nz b
    sub a b = NVector x y z
      where
        x = nx a - nx b
        y = ny a - ny b
        z = nz a - nz b
    zero = NVector 0 0 0

-- | 'NVector' norm.
instance Norm NVector Double where
  norm a = sqrt ((nx a * nx a) + (ny a * ny a) + (nz a * nz a))

-- | Computes the cross product of the two given 'NVector's.
cross :: NVector -> NVector -> NVector
cross a b = NVector x y z
  where
    x = ny a * nz b - nz a * ny b
    y = nz a * nx b - nx a * nz b
    z = nx a * ny b - ny a * nx b

-- | Computes the dot product of the two given 'NVector's.
dot :: NVector -> NVector -> Double
dot a b = nx a * nx b + ny a * ny b + nz a * nz b

-- | @rotate nv rm@ applies rotation matrix @rm@ to 'NVector' @nv@.
rotate :: NVector -> [NVector] -> NVector
rotate nv rm
    | length rm /= 3 = error ("Invalid rotation matrix" ++ show rm)
    | otherwise = NVector nx' ny' nz'
       where
           [nx', ny', nz'] = map (dot nv) rm

-- | Multiplies each component of the given 'NVector' by the given value.
scale :: NVector -> Double -> NVector
scale a s = NVector x y z
  where
    x = nx a * s
    y = ny a * s
    z = nz a * s

-- | Normalises the given 'NVector'.
unit :: NVector -> NVector
unit a
    | s == 1.0 = a
    | otherwise = scale a s
  where
    s = 1.0 / norm a
