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
    , nvector
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Vector3d

-- | Represents a position as the normal vector to the sphere.
--
-- Orientation: z-axis points to the North Pole along the Earth's rotation axis,
-- x-axis points towards the point where latitude = longitude = 0.
newtype NVector =
    NVector Vector3d
    deriving (Eq, Show)

instance IsVector3d NVector where
    vec (NVector v) = v

-- | Unit 'NVector' from given x, y and z.
nvector :: Double -> Double -> Double -> NVector
nvector x y z = NVector (vunit (Vector3d x y z))

-- | Horizontal position of the North Pole.
northPole :: NVector
northPole = NVector (Vector3d 0.0 0.0 1.0)

-- | Horizontal position of the North Pole.
southPole :: NVector
southPole = NVector (Vector3d 0.0 0.0 (-1.0))
