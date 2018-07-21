-- |
-- Module:      Data.Geo.Jord.NEDVector
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with vectors the NED (north, east and down) coordinates system.
--
-- TODO: add fromLengthBearingElevation
--
module Data.Geo.Jord.Ellipsoidal.NedVector
    ( NedVector(north, east, down)
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
data NedVector = NedVector
    { north :: Length
    , east :: Length
    , down :: Length
    } deriving (Show)

-- | 'NedVector' from given north, east and down.
nedVector :: Length -> Length -> Length -> NedVector
nedVector = NedVector

-- | @length v@ computes the length of the NED vector @v@.
length :: NedVector -> Length
length p = metres (sqrt (n * n + e * e + d * d))
  where
    n = toMetres (north p)
    e = toMetres (east p)
    d = toMetres (down p)

-- | @bearing v@ computes the bearing of the NED vector @v@ from north.
bearing :: NedVector -> Angle
bearing p =
    let a = atan2' (toMetres (east p)) (toMetres (north p))
     in normalise a (decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED vector @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: NedVector -> Angle
elevation p = negate' (asin' (toMetres (down p) / toMetres (length p)))
