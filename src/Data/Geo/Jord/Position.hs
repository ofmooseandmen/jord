-- |
-- Module:      Data.Geo.Jord.GreatCircle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with positions.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- This module assumes a spherical earth.
--
module Data.Geo.Jord.Position
    (
    -- * The 'Position' type
      Position(..)
    -- * Geodetic calculations
    , angularDistance
    , antipode
    , destination
    , destination'
    , distance
    , distance'
    , finalBearing
    , initialBearing
    , interpolate
    , isInside
    , mean
    -- * Misc.
    , meanEarthRadius
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.List (subsequences)
import Prelude hiding (fail)

-- | The 'Position' class defines 2 functions to convert a position to and from a 'NVector'.
-- All functions in this module first convert 'Position' to 'NVector' and any resulting 'NVector' back
-- to a 'Position'. This allows the call site to pass either 'NVector' or 'LatLong' and to get back
-- the same class instance.
class Position a where
    -- | Converts a 'NVector' into 'Position' instance.
    fromNVector :: NVector -> a
    -- | Converts the 'Position' instance into a 'NVector'.
    toNVector :: a -> NVector

-- | 'LatLong' to/from 'NVector'.
instance Position LatLong where
    fromNVector v = latLong lat lon
      where
        lat = atan2' (z v) (sqrt (x v * x v + y v * y v))
        lon = atan2' (y v) (x v)
    toNVector g = nvector x' y' z'
      where
        lat = latitude g
        lon = longitude g
        cl = cos' lat
        x' = cl * cos' lon
        y' = cl * sin' lon
        z' = sin' lat

-- | Identity.
instance Position NVector where
    fromNVector v = v
    toNVector v = v

-- | Angle between the two given 'NVector's.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
angularDistance :: NVector -> NVector -> Maybe NVector -> Angle
angularDistance v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2

-- | Returns the antipodal 'Position' of the given 'Position' - i.e. the position on the surface
-- of the Earth which is diametrically opposite to the given position.
antipode :: (Position a) => a -> a
antipode p = fromNVector (scale (toNVector p) (-1.0))

-- | 'destination'' assuming a radius of 'meanEarthRadius'.
destination :: (Position a) => a -> Angle -> Length -> a
destination p b d = destination' p b d meanEarthRadius

-- | Computes the destination 'Position' from the given 'Position' having travelled the given distance on the
-- given initial bearing (bearing will normally vary before destination is reached) and using the given earth radius.
--
-- This is known as the direct geodetic problem.
destination' :: (Position a) => a -> Angle -> Length -> Length -> a
destination' p b d r
    | isZero d = p
    | otherwise = fromNVector (add (scale v (cos' ta)) (scale de (sin' ta)))
  where
    v = toNVector p
    ed = unit (cross northPole v) -- east direction vector at v
    nd = cross v ed -- north direction vector at v
    ta = central d r -- central angle
    de = add (scale nd (cos' b)) (scale ed (sin' b)) -- unit vector in the direction of the azimuth

-- | 'distance'' assuming a radius of 'meanEarthRadius'.
distance :: (Position a) => a -> a -> Length
distance p1 p2 = distance' p1 p2 meanEarthRadius

-- | Computes the surface distance (length of geodesic) in 'Meters' assuming a
-- spherical Earth between the two given 'Position's and using the given earth radius.
distance' :: (Position a) => a -> a -> Length -> Length
distance' p1 p2 = arcLength (angularDistance v1 v2 Nothing)
  where
    v1 = toNVector p1
    v2 = toNVector p2

-- | Computes the final bearing arriving at given destination  @p2@ 'Position' from given 'Position' @p1@.
--  the final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
-- Returns 180 if both position are equals.
finalBearing :: (Position a) => a -> a -> Angle
finalBearing p1 p2 = normalise (initialBearing p2 p1) (decimalDegrees 180)

-- | Computes the initial bearing from given @p1@ 'Position' to given @p2@ 'Position', in compass degrees.
-- Returns 0 if both position are equals.
initialBearing :: (Position a) => a -> a -> Angle
initialBearing p1 p2 = normalise (angularDistance gc1 gc2 (Just v1)) (decimalDegrees 360)
  where
    v1 = toNVector p1
    v2 = toNVector p2
    gc1 = cross v1 v2 -- great circle through p1 & p2
    gc2 = cross v1 northPole -- great circle through p1 & north pole

-- | Computes the 'Position' at given fraction @f@ between the two given 'Position's @p0@ and @p1@.
--
-- Special conditions:
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

-- | Determines whether the given 'Position' is inside the polygon defined by the given list of 'Position's.
-- The polygon is closed if needed (i.e. if @head ps /= last ps@).
--
-- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
-- will sum to less than 360°, and exterior point angles will be small but non-zero.
--
-- Always returns 'False' if positions does not at least defines a triangle.
--
isInside :: (Eq a, Position a) => a -> [a] -> Bool
isInside p ps
    | null ps = False
    | head ps == last ps = isInside p (init ps)
    | length ps < 3 = False
    | otherwise =
        let aSum = foldl (\a v' -> add a (uncurry angularDistance v' (Just v))) (decimalDegrees 0) es
         in abs (toDecimalDegrees aSum) > 180.0
  where
    v = toNVector p
    es = egdes (map (sub v . toNVector) ps)

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [NVector] -> [(NVector, NVector)]
egdes ps = zip ps ps'
  where
    ps' = tail ps ++ [head ps]

-- | Computes the geographic mean 'Position' of the given 'Position's if it is defined.
--
-- The geographic mean is not defined for the antipodals positions (since they
-- cancel each other).
--
-- Special conditions:
--
-- @
--     mean [] == Nothing
--     mean [p] == Just p
--     mean [p1, p2, p3] == Just circumcentre
--     mean [p1, .., antipode p1] == Nothing
-- @
--
mean :: (Position a) => [a] -> Maybe a
mean [] = Nothing
mean [p] = Just p
mean ps =
    if null antipodals
        then Just (fromNVector (unit (foldl add zero vs)))
        else Nothing
  where
    vs = map toNVector ps
    ts = filter (\l -> length l == 2) (subsequences vs)
    antipodals =
        filter
            (\t -> (fromNVector (antipode (head t)) :: LatLong) == (fromNVector (last t) :: LatLong))
            ts

-- | Mean Earth radius: 6,371,008.8 metres.
meanEarthRadius :: Length
meanEarthRadius = metres 6371008.8

-- | 'Position' of the North Pole.
northPole :: (Position a) => a
northPole = fromNVector (nvector 0.0 0.0 1.0)

-- | 'Position' of the South Pole.
southPole :: (Position a) => a
southPole = fromNVector (nvector 0.0 0.0 (-1.0))
