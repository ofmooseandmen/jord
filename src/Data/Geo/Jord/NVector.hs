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
    ) where

import Data.Geo.Jord.Vector3d

-- | Represents a position as the normal vector to the sphere.
--
-- Orientation: z-axis points to the North Pole along the Earth's rotation axis,
-- x-axis points towards the point where latitude = longitude = 0.
data NVector = NVector
    { nx :: Double
    , ny :: Double
    , nz :: Double
    } deriving (Eq, Show)

-- | Unit 'NVector' from given x, y and z.
nvector :: Double -> Double -> Double -> NVector
nvector x y z = vunit (NVector x y z)

instance Vector3d NVector where
    vnorm v = sqrt ((nx v * nx v) + (ny v * ny v) + (nz v * nz v))
    vecx = nx
    vecy = ny
    vecz = nz
    vector3d = NVector
