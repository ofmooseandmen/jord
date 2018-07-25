{-# LANGUAGE MultiParamTypeClasses #-}

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
    ( NedVector(..)
    , nedVectorMetres
    , bearing
    , elevation
    , norm
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Length
import Data.Geo.Jord.Vector3d

-- | North east down (NED), also known as local tangent plane (LTP):
-- a vector in the local coordinate frame of a body.
data NedVector = NedVector
    { north :: Length
    , east :: Length
    , down :: Length
    } deriving (Eq, Show)

instance Vector3d NedVector where
    vecx v = toMetres (north v)
    vecy v = toMetres (east v)
    vecz v = toMetres (down v)
    vector3d = nedVectorMetres

-- | 'NedVector' from given north, east and down in _metres_.
nedVectorMetres :: Double -> Double -> Double -> NedVector
nedVectorMetres n e d = NedVector (metres n) (metres e) (metres d)

-- | @bearing v@ computes the bearing of the NED vector @v@ from north.
bearing :: NedVector -> Angle
bearing v =
    let a = atan2' (toMetres (east v)) (toMetres (north v))
     in normalise a (decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED vector @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: NedVector -> Angle
elevation v = negate' (asin' (toMetres (down v) / vnorm v))

-- | @norm v@ computes the norm of the NED vector @v@.
norm :: NedVector -> Length
norm v = metres (vnorm v)
