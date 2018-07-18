-- |
-- Module:      Data.Geo.Jord.Spherical.Geodetics
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geodetics calculations on horizontal positions assuming a spherical earth model.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Spherical.Geodetics
    ( angularDistance
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
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Position (Geodetic2D(..), northPole)
import Data.Geo.Jord.Quantity
import Data.List (subsequences)
import Prelude hiding (fail)

-- | @angularDistance p1 p2 n@ computes the angle between the horizontal positions @p1@ and @p2@.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
angularDistance :: (Geodetic2D a) => a -> a -> Maybe a -> Angle
angularDistance p1 p2 n = angularDistance' (toNVector p1) (toNVector p2) (fmap toNVector n)

-- | Angle between the two given n-vectors.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
angularDistance' :: NVector -> NVector -> Maybe NVector -> Angle
angularDistance' v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2

-- | @antipode p@ computes the antipodal horizontal position of @p@:
-- the horizontal position on the surface of the Earth which is diametrically opposite to @p@.
antipode :: (Geodetic2D a) => a -> a
antipode p = fromNVector (scale (toNVector p) (-1.0))

-- | 'destination'' using the mean radius of the WGS84 reference ellipsoid.
destination :: (Geodetic2D a) => a -> Angle -> Length -> a
destination p b d = destination' p b d meanEarthRadius

-- | @destination' p b d r@ computes the destination horizontal position from position @p@ having
-- travelled the distance @d@ on the initial bearing @b@ (bearing will normally vary
-- before destination is reached) and using the earth radius @r@.
--
-- This is known as the direct geodetic problem.
destination' :: (Geodetic2D a) => a -> Angle -> Length -> Length -> a
destination' p b d r
    | isZero d = p
    | otherwise = fromNVector (add (scale v (cos' ta)) (scale de (sin' ta)))
  where
    v = toNVector p
    ed = unit (cross northPole v) -- east direction vector at v
    nd = cross v ed -- north direction vector at v
    ta = central d r -- central angle
    de = add (scale nd (cos' b)) (scale ed (sin' b)) -- unit vector in the direction of the azimuth

-- | 'distance'' using the mean radius of the WGS84 reference ellipsoid.
distance :: (Geodetic2D a) => a -> a -> Length
distance p1 p2 = distance' p1 p2 meanEarthRadius

-- | @distance' p1 p2@ computes the surface distance (length of geodesic) between the positions @p1@ and @p2@
-- and using the earth radius @r@.
distance' :: (Geodetic2D a) => a -> a -> Length -> Length
distance' p1 p2 = arcLength (angularDistance p1 p2 Nothing)

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
--  The final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
-- Returns 180 if both horizontal positions are equals.
finalBearing :: (Geodetic2D a) => a -> a -> Angle
finalBearing p1 p2 = normalise (initialBearing p2 p1) (decimalDegrees 180)

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
-- Returns 0 if both horizontal positions are equals.
initialBearing :: (Geodetic2D a) => a -> a -> Angle
initialBearing p1 p2 = normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
  where
    v1 = toNVector p1
    v2 = toNVector p2
    gc1 = cross v1 v2 -- great circle through p1 & p2
    gc2 = cross v1 northPole -- great circle through p1 & north pole

-- | @interpolate p0 p1 f# computes the horizontal position at fraction @f@ between the @p0@ and @p1@.
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
interpolate :: (Geodetic2D a) => a -> a -> Double -> a
interpolate p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = fromNVector (unit (add v0 (scale (sub v1 v0) f)))
  where
    v0 = toNVector p0
    v1 = toNVector p1

-- | @isInside p ps@ determines whether the @p@ is inside the polygon defined by the list of positions @ps@.
-- The polygon is closed if needed (i.e. if @head ps /= last ps@).
--
-- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
-- will sum to less than 360°, and exterior point angles will be small but non-zero.
--
-- Always returns 'False' if @ps@ does not at least defines a triangle.
--
isInside :: (Eq a, Geodetic2D a) => a -> [a] -> Bool
isInside p ps
    | null ps = False
    | head ps == last ps = isInside p (init ps)
    | length ps < 3 = False
    | otherwise =
        let aSum =
                foldl (\a v' -> add a (uncurry angularDistance v' (Just v))) (decimalDegrees 0) es
         in abs (toDecimalDegrees aSum) > 180.0
  where
    v = toNVector p
    es = egdes (map (sub v . toNVector) ps)

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [NVector] -> [(NVector, NVector)]
egdes ps = zip ps ps'
  where
    ps' = tail ps ++ [head ps]

-- | @mean ps@ computes the mean geographic horitzontal position given list of position @ps@, if it is defined.
--
-- The geographic mean is not defined for antipodals position (since they
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
mean :: (Geodetic2D a) => [a] -> Maybe a
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

-- | WGS84 ellipsoid mean radius.
meanEarthRadius :: Length
meanEarthRadius = meanRadius wgs84
