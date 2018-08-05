-- |
-- Module:      Data.Geo.Jord.NedVector
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
module Data.Geo.Jord.NedVector
    ( NedVector
    , ned
    , nedMetres
    , north
    , east
    , down
    , bearing
    , elevation
    , norm
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Length
import Data.Geo.Jord.Vector3d

-- | North east down (NED), also known as local tangent plane (LTP):
-- a vector in the local coordinate frame of a body.
newtype NedVector =
    NedVector Vector3d
    deriving (Eq, Show)

instance IsVector3d NedVector where
    vec (NedVector v) = v

-- | 'NedVector' from given north, east and down.
ned :: Length -> Length -> Length -> NedVector
ned n e d = NedVector (Vector3d (toMetres n) (toMetres e) (toMetres d))

-- | 'NedVector' from given north, east and down in _metres_.
nedMetres :: Double -> Double -> Double -> NedVector
nedMetres n e d = ned (metres n) (metres e) (metres d)

-- | North component of given 'NedVector'.
north :: NedVector -> Length
north (NedVector v) = metres (vx v)

-- | East component of given 'NedVector'.
east :: NedVector -> Length
east (NedVector v) = metres (vy v)

-- | Down component of given 'NedVector'.
down :: NedVector -> Length
down (NedVector v) = metres (vz v)

-- | @bearing v@ computes the bearing of the NED vector @v@ from north.
bearing :: NedVector -> Angle
bearing v =
    let a = atan2' (toMetres (east v)) (toMetres (north v))
     in normalise a (decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED vector @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: NedVector -> Angle
elevation (NedVector v) = negate' (asin' (vz v / vnorm v))

-- | @norm v@ computes the norm of the NED vector @v@.
norm :: NedVector -> Length
norm (NedVector v) = metres (vnorm v)
