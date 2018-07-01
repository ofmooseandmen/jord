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
--     * crossTrackDistance :: Position -> GreatCircle -> Meters
--
--     * alongTrackDistance :: Position -> GreatArc -> Meters
--
--     * isWithin :: [Position] -> Bool
--
--     * nearestPoint :: Position -> GreatArc -> Position
--
--     * area :: [Position] -> SquareMeters
--
--     * closestApproach
--
module Data.Geo.Jord.GreatCircle
    ( GreatCircle
    , Meters(..)
    , Millis(..)
    , Position
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

import Data.Fixed
import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.NVector
import Prelude hiding (subtract)

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

-- | A distance in meters.
newtype Meters = Meters
    { meters :: Double
    } deriving (Eq, Show)

-- | An instant or duration in milliseconds.
newtype Millis = Millis
    { millis :: Int
    } deriving (Eq, Ord, Show)

-- | The 'Position' class defines 2 functions to convert a position to and from a 'NVector'.
-- All functions in this module first convert 'Position' to 'NVector' and any resulting 'NVector' back
-- to a 'Position'. This allows the call site to pass either 'NVector' or 'GeoPos' and to get back
-- the same class instance.
class Position a where
    fromNVector :: NVector -> a
    toNVector :: a -> NVector

-- | 'GeoPos' to/from 'NVector'.
instance Position GeoPos where
    fromNVector v = geo (toDegrees lat) (toDegrees lon)
      where
        lat = atan2 (z v) (sqrt (x v * x v + y v * y v))
        lon = atan2 (y v) (x v)
    toNVector g = nvector x' y' z'
      where
        lat = toRadians (latitude g)
        lon = toRadians (longitude g)
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
destination :: (Position a) => a -> Degrees -> Meters -> a
destination p _ (Meters 0) = p
destination p b d = fromNVector (add (scale v (cos ta)) (scale de (sin ta)))
  where
    v = toNVector p
    ed = normalise (cross northPole v) -- east direction vector at v
    nd = cross v ed -- north direction vector at v
    a = toRadians b -- azimuth in radians
    ta = meters d / meters meanEarthRadius -- angle travelled in radians
    de = add (scale nd (cos a)) (scale ed (sin a)) -- unit vector in the direction of the azimuth

-- | Computes the surface distance (length of geodesic) in 'Meters' assuming a
-- spherical Earth between the two given 'Position's.
distance :: (Position a) => a -> a -> Meters
distance p1 p2 = Meters (meters meanEarthRadius * angle v1 v2 Nothing)
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
greatCircleBearing :: (Position a) => a -> Degrees -> GreatCircle
greatCircleBearing p b = GreatCircle (subtract n' e')
  where
    rad = toRadians b
    v = toNVector p
    e = cross northPole v -- easting
    n = cross v e -- northing
    e' = scale e (cos rad / norm e)
    n' = scale n (sin rad / norm n)

-- | Computes the final bearing arriving at given destination  @p2@ 'Position' from given 'Position' @p1@.
--  the final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
-- Returns 180 if both position are equals.
finalBearing :: (Position a) => a -> a -> Degrees
finalBearing p1 p2 = Degrees (mod' (d + 180.0) 360.0)
  where
    d = degrees (initialBearing p2 p1)

-- | Computes the initial bearing from given @p1@ 'Position' to given @p2@ 'Position', in compass degrees.
-- Returns 0 if both position are equals.
initialBearing :: (Position a) => a -> a -> Degrees
initialBearing p1 p2 = normaliseDegrees (toDegrees (angle gc1 gc2 (Just v1)))
  where
    v1 = toNVector p1
    v2 = toNVector p2
    gc1 = cross v1 v2 -- great circle through p1 & p2
    gc2 = cross v1 northPole -- great circle through p1 & north pole

-- | Computes the interpolated 'Position' at time @ti@,
-- knowing the 'Position' @p0@ at time @t0@ and the 'Position' @p1@ at time @t1@.
--
-- Expects @t0@ <= @ti@ <= @t1@
interpolate :: (Position a) => a -> Millis -> a -> Millis -> Millis -> a
interpolate p0 t0 p1 t1 ti
    | ti < t0 || ti > t1 || t0 > t1 = error "expected t0 <= ti <= t1"
    | ti == t0 = p0
    | ti == t1 = p1
    | otherwise = fromNVector (normalise (add v0 (scale (subtract v1 v0) s)))
  where
    v0 = toNVector p0
    v1 = toNVector p1
    s = fromIntegral (millis ti - millis t0) / fromIntegral (millis t1 - millis t0)

-- | Computes the intersections between the two given 'GreatCircle's.
-- Two 'GreatCircle's intersect exactly twice unless there are equal, in which case 'Nothing' is returned.
intersections :: (Position a) => GreatCircle -> GreatCircle -> Maybe (a, a)
intersections gc1 gc2
    | norm i == 0.0 = Nothing
    | otherwise
    , let ni = normalise i = Just (fromNVector ni, fromNVector (antipode ni))
  where
    i = cross (normal gc1) (normal gc2)

-- | Mean Earth radius in meters.
meanEarthRadius :: Meters
meanEarthRadius = Meters 6371008.8

-- | Computes the mid 'Position' between the given list of 'Position's which must be non-empty.
midpoint :: (Position a) => [a] -> a
midpoint [] = error "midpoint expects a non-empty list"
midpoint [p] = p
midpoint ps = fromNVector (normalise (foldl add zero vs))
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
angle :: NVector -> NVector -> Maybe NVector -> Double
angle v1 v2 n = atan2 sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2

-- | normalise to [0, 360].
normaliseDegrees :: Double -> Degrees
normaliseDegrees d = Degrees (mod' (d + 360.0) 360.0)

-- | degrees to radians.
toRadians :: Degrees -> Double
toRadians d = degrees d * pi / 180.0

-- | radians to degrees.
toDegrees :: Double -> Double
toDegrees r = r / pi * 180.0
