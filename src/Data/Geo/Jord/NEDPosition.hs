-- |
-- Module:      Data.Geo.Jord.NEDVector
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working position in the NED (north, east and down) coordinates system.
--
module Data.Geo.Jord.NEDPosition
    ( NEDPosition(north, east, down)
    , nedPosition
    , length
    , bearing
    , elevation
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Length
import Prelude hiding (length)

-- | North east down (NED) position, also known as local tangent plane (LTP):
-- a vector in the local coordinate frame of a body.
data NEDPosition = NEDPosition
    { north :: Length
    , east :: Length
    , down :: Length
    } deriving (Show)

-- | 'NEDPosition' from given north, east and down.
nedPosition :: Length -> Length -> Length -> NEDPosition
nedPosition = NEDPosition

-- | @length v@ computes the length of the NED position @v@.
length :: NEDPosition -> Length
length p = metres (sqrt (n * n + e * e + d * d))
  where
    n = toMetres (north p)
    e = toMetres (east p)
    d = toMetres (down p)

-- | @bearing v@ computes the bearing of the NED position @v@ from north.
bearing :: NEDPosition -> Angle
bearing p =
    let a = atan2' (toMetres (east p)) (toMetres (north p))
     in normalise a (decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED position @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: NEDPosition -> Angle
elevation p = negate' (asin' (toMetres (down p) / toMetres (length p)))
