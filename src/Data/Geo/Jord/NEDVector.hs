-- |
-- Module:      Data.Geo.Jord.NEDVector
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with north/east/down vectors.
--
module Data.Geo.Jord.NEDVector
    ( NEDVector
    , nedVector
    , length
    , bearing
    , elevation
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Length
import Prelude hiding (length)

-- | North east down (NED), also known as local tangent plane (LTP):
-- a vector in the local coordinate frame of a body.
data NEDVector = NEDVector
    { north :: Length
    , east :: Length
    , down :: Length
    } deriving (Show)

-- | 'NEDVector' from given north, east and down.
nedVector :: Length -> Length -> Length -> NEDVector
nedVector = NEDVector

-- | @length v@ computes the length of the NED vector @v@.
length :: NEDVector -> Length
length v = metres (sqrt (n * n + e * e + d * d))
  where
    n = toMetres (north v)
    e = toMetres (east v)
    d = toMetres (down v)

-- | @bearing v@ computes the bearing of the NED vector @v@ from north.
bearing :: NEDVector -> Angle
bearing v =
    let a = atan2' (toMetres (east v)) (toMetres (north v))
     in normalise a (decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED vector @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: NEDVector -> Angle
elevation v = negate' (asin' (toMetres (down v) / toMetres (length v)))
