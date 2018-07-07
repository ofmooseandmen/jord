-- |
-- Module:      Data.Geo.Jord.GreatCircle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with <https://en.wikipedia.org/wiki/Great_circle Great Circle>.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- This module assumes a spherical earth with a radius of 6371008.8 meters.
--
-- TODO:
--
--     * crossTrackDistance :: Position -> GreatCircle -> Distance
--
--     * alongTrackDistance :: Position -> GreatArc -> Distance
--
--     * isWithin :: [Position] -> Bool
--
--     * nearestPoint :: Position -> GreatArc -> Position
--
--     * area :: [Position] -> Surface
--
--     * closestApproach
--
module Data.Geo.Jord.GreatCircle
    ( GreatCircle
    , Position(..)
    , antipode
    , destination
    , distance
    , finalBearing
    , greatCircle
    , greatCircleBearing
    , initialBearing
    , interpolate
    , intersections
    , meanEarthRadius
    , midpoint
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity

-- | A circle on the surface of the Earth which lies in a plane passing through
-- the Earth's centre.
--
-- It is internally represented as its normal vector - i.e. the normal vector
-- to the plane containing the great circle.
--
-- Use either 'greatCircle' or 'greatCircleBearing' constructors.
--
newtype GreatCircle = GreatCircle
    { normal :: NVector
    }

-- | The 'Position' class defines 2 functions to convert a position to and from a 'NVector'.
-- All functions in this module first convert 'Position' to 'NVector' and any resulting 'NVector' back
-- to a 'Position'. This allows the call site to pass either 'NVector' or 'GeoPos' and to get back
-- the same class instance.
class Position a where
    -- | Converts a 'NVector' into 'Position' instance.
    fromNVector :: NVector -> a
    -- | Converts the 'Position' instance into a 'NVector'.
    toNVector :: a -> NVector

-- | 'GeoPos' to/from 'NVector'.
instance Position GeoPos where
    fromNVector v = geoPos (toDegrees lat) (toDegrees lon)
      where
        lat = atan2 (z v) (sqrt (x v * x v + y v * y v))
        lon = atan2 (y v) (x v)
    toNVector g = nvector x' y' z'
      where
        lat = radians (latitude g)
        lon = radians (longitude g)
        cl = cos lat
        x' = cl * cos lon
        y' = cl * sin lon
        z' = sin lat

-- | Identity.
instance Position NVector where
    fromNVector v = v
    toNVector v = v

-- | Returns the antipodal 'Position' of the given 'Position' - i.e. the position on the surface
-- of the Earth which is diametrically opposite to the given position.
antipode :: (Position a) => a -> a
antipode p = fromNVector (scale (toNVector p) (-1.0))

-- | Computes the destination 'Position' from the given 'Position' having travelled the given distance on the
-- given initial bearing (bearing will normally vary before destination is reached).
--
-- This is known as the direct geodetic problem.
destination :: (Position a) => a -> Angle -> Length -> a
destination p b d
    | metres d == 0.0 = p
    | otherwise = fromNVector (add (scale v (cos ta)) (scale de (sin ta)))
  where
    v = toNVector p
    ed = unit (cross northPole v) -- east direction vector at v
    nd = cross v ed -- north direction vector at v
    a = radians b -- azimuth in radians
    ta = metres d / metres meanEarthRadius -- angle travelled in radians
    de = add (scale nd (cos a)) (scale ed (sin a)) -- unit vector in the direction of the azimuth

-- | Computes the surface distance (length of geodesic) in 'Meters' assuming a
-- spherical Earth between the two given 'Position's.
distance :: (Position a) => a -> a -> Length
distance p1 p2 = ofMetres (metres meanEarthRadius * angleBetween v1 v2 Nothing)
  where
    v1 = toNVector p1
    v2 = toNVector p2

-- | Returns a 'GreateCircle' passing by both given 'Position's.
greatCircle :: (Position a) => a -> a -> GreatCircle
greatCircle p1 p2 = GreatCircle (cross v1 v2)
  where
    v1 = toNVector p1
    v2 = toNVector p2

-- | Returns a 'GreatCircle' passing by the given 'Position' and heading on given bearing.
greatCircleBearing :: (Position a) => a -> Angle -> GreatCircle
greatCircleBearing p b = GreatCircle (sub n' e')
  where
    rad = radians b
    v = toNVector p
    e = cross northPole v -- easting
    n = cross v e -- northing
    e' = scale e (cos rad / norm e)
    n' = scale n (sin rad / norm n)

-- | Computes the final bearing arriving at given destination  @p2@ 'Position' from given 'Position' @p1@.
--  the final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
-- Returns 180 if both position are equals.
finalBearing :: (Position a) => a -> a -> Angle
finalBearing p1 p2 = normalise (initialBearing p2 p1) 180.0

-- | Computes the initial bearing from given @p1@ 'Position' to given @p2@ 'Position', in compass degrees.
-- Returns 0 if both position are equals.
initialBearing :: (Position a) => a -> a -> Angle
initialBearing p1 p2 = normalise (ofRadians (angleBetween gc1 gc2 (Just v1))) 360.0
  where
    v1 = toNVector p1
    v2 = toNVector p2
    gc1 = cross v1 v2 -- great circle through p1 & p2
    gc2 = cross v1 northPole -- great circle through p1 & north pole

-- | Computes the 'Position' at given fraction @f@ between the two given 'Position's @p0@ and @p1@.
--
-- Special cases:
--
-- @
--     interpolate p0 p1 0.0 => p0
--     interpolate p0 p1 1.0 => p1
-- @
--
-- 'error's if @f < 0 || f > 1.0@
--
interpolate :: (Position a) => a -> a -> Double -> a
interpolate p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = fromNVector (unit (add v0 (scale (sub v1 v0) f)))
  where
    v0 = toNVector p0
    v1 = toNVector p1

-- | Computes the intersections between the two given 'GreatCircle's.
-- Two 'GreatCircle's intersect exactly twice unless there are equal, in which case 'Nothing' is returned.
intersections :: (Position a) => GreatCircle -> GreatCircle -> Maybe (a, a)
intersections gc1 gc2
    | norm i == 0.0 = Nothing
    | otherwise
    , let ni = unit i = Just (fromNVector ni, fromNVector (antipode ni))
  where
    i = cross (normal gc1) (normal gc2)

-- | Mean Earth radius.
meanEarthRadius :: Length
meanEarthRadius = ofMetres 6371008.8

-- | Computes the mid 'Position' between the given list of 'Position's which must be non-empty.
midpoint :: (Position a) => [a] -> a
midpoint [] = error "midpoint expects a non-empty list"
midpoint [p] = p
midpoint ps = fromNVector (unit (foldl add zero vs))
  where
    vs = map toNVector ps

-- | 'Position' of the North Pole.
northPole :: (Position a) => a
northPole = fromNVector (nvector 0.0 0.0 1.0)

-- | 'Position' of the South Pole.
southPole :: (Position a) => a
southPole = fromNVector (nvector 0.0 0.0 (-1.0))

-- | Angle bteween the tow given 'NVector's.
-- If @n@ is 'Nothing', the angle is always in [0..PI], otherwise it is in [-PI, +PI],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
angleBetween :: NVector -> NVector -> Maybe NVector -> Double
angleBetween v1 v2 n = atan2 sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2
