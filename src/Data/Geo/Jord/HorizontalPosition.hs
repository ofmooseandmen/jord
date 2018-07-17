-- |
-- Module:      Data.Geo.Jord.GreatCircle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with horizontal positions.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- This module assumes a spherical earth.
--
module Data.Geo.Jord.HorizontalPosition
    (
    -- * The 'HorizontalPosition' type
      HorizontalPosition(..)
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
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Vector3d
import Data.List (subsequences)
import Prelude hiding (fail)

-- | The 'HorizontalPosition' class defines 2 functions to convert a Horizontal Position to and from a n-vector.
-- All functions in this module first convert 'HorizontalPosition' to a n-vector and any resulting n-vector back
-- to a 'HorizontalPosition'. This allows the call site to pass either n-vector or another 'HorizontalPosition' instance
-- and to get back the same class instance.
class (Eq a) => HorizontalPosition a where
    -- | Converts a 'Vector3d' into 'HorizontalPosition' instance.
    fromNVector :: Vector3d -> a
    -- | Converts the 'HorizontalPosition' instance into a 'Vector3d'.
    toNVector :: a -> Vector3d

-- | 'LatLong' to/from 'Vector3d'.
instance HorizontalPosition LatLong where
    fromNVector v = latLong lat lon
      where
        lat = atan2' (z v) (sqrt (x v * x v + y v * y v))
        lon = atan2' (y v) (x v)
    toNVector g = Vector3d x' y' z'
      where
        lat = latitude g
        lon = longitude g
        cl = cos' lat
        x' = cl * cos' lon
        y' = cl * sin' lon
        z' = sin' lat

-- | Identity.
instance HorizontalPosition Vector3d where
    fromNVector v = v
    toNVector v = v

-- | Angle between the two given 'HorizontalPosition's.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
angularDistance :: (HorizontalPosition a) => a -> a -> Maybe a -> Angle
angularDistance p1 p2 n = angularDistance' (toNVector p1) (toNVector p2) (fmap toNVector n)

-- | Angle between the two given 'Vector3d's.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
angularDistance' :: Vector3d -> Vector3d -> Maybe Vector3d -> Angle
angularDistance' v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2

-- | Returns the antipodal 'HorizontalPosition' of the given 'HorizontalPosition' - i.e. the HorizontalPosition on the surface
-- of the Earth which is diametrically opposite to the given HorizontalPosition.
antipode :: (HorizontalPosition a) => a -> a
antipode p = fromNVector (scale (toNVector p) (-1.0))

-- | 'destination'' assuming a radius of 'meanEarthRadius'.
destination :: (HorizontalPosition a) => a -> Angle -> Length -> a
destination p b d = destination' p b d meanEarthRadius

-- | Computes the destination 'HorizontalPosition' from the given 'HorizontalPosition' having travelled the given distance on the
-- given initial bearing (bearing will normally vary before destination is reached) and using the given earth radius.
--
-- This is known as the direct geodetic problem.
destination' :: (HorizontalPosition a) => a -> Angle -> Length -> Length -> a
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
distance :: (HorizontalPosition a) => a -> a -> Length
distance p1 p2 = distance' p1 p2 meanEarthRadius

-- | Computes the surface distance (length of geodesic) in 'Meters' assuming a
-- spherical Earth between the two given 'HorizontalPosition's and using the given earth radius.
distance' :: (HorizontalPosition a) => a -> a -> Length -> Length
distance' p1 p2 = arcLength (angularDistance p1 p2 Nothing)

-- | Computes the final bearing arriving at given destination  @p2@ 'HorizontalPosition' from given 'HorizontalPosition' @p1@.
--  the final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
-- Returns 180 if both HorizontalPosition are equals.
finalBearing :: (HorizontalPosition a) => a -> a -> Angle
finalBearing p1 p2 = normalise (initialBearing p2 p1) (decimalDegrees 180)

-- | Computes the initial bearing from given @p1@ 'HorizontalPosition' to given @p2@ 'HorizontalPosition', in compass degrees.
-- Returns 0 if both HorizontalPosition are equals.
initialBearing :: (HorizontalPosition a) => a -> a -> Angle
initialBearing p1 p2 = normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
  where
    v1 = toNVector p1
    v2 = toNVector p2
    gc1 = cross v1 v2 -- great circle through p1 & p2
    gc2 = cross v1 northPole -- great circle through p1 & north pole

-- | Computes the 'HorizontalPosition' at given fraction @f@ between the two given 'HorizontalPosition's @p0@ and @p1@.
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
interpolate :: (HorizontalPosition a) => a -> a -> Double -> a
interpolate p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = fromNVector (unit (add v0 (scale (sub v1 v0) f)))
  where
    v0 = toNVector p0
    v1 = toNVector p1

-- | Determines whether the given 'HorizontalPosition' is inside the polygon defined by the given list of 'HorizontalPosition's.
-- The polygon is closed if needed (i.e. if @head ps /= last ps@).
--
-- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
-- will sum to less than 360°, and exterior point angles will be small but non-zero.
--
-- Always returns 'False' if HorizontalPositions does not at least defines a triangle.
--
isInside :: (Eq a, HorizontalPosition a) => a -> [a] -> Bool
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
egdes :: [Vector3d] -> [(Vector3d, Vector3d)]
egdes ps = zip ps ps'
  where
    ps' = tail ps ++ [head ps]

-- | Computes the geographic mean 'HorizontalPosition' of the given 'HorizontalPosition's if it is defined.
--
-- The geographic mean is not defined for the antipodals HorizontalPositions (since they
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
mean :: (HorizontalPosition a) => [a] -> Maybe a
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

-- | 'HorizontalPosition' of the North Pole.
northPole :: (HorizontalPosition a) => a
northPole = fromNVector (Vector3d 0.0 0.0 1.0)

-- | 'HorizontalPosition' of the South Pole.
southPole :: (HorizontalPosition a) => a
southPole = fromNVector (Vector3d 0.0 0.0 (-1.0))
