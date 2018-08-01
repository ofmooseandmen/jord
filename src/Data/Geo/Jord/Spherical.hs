{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Data.Geo.Jord.Spherical
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geodetic calculations assuming a __spherical__ earth model.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Spherical
    ( SGeodetics(angularDistance, antipode, destination, destination84,
           finalBearing, initialBearing, interpolate, insideSurface, mean,
           surfaceDistance, surfaceDistance84)
    ) where

import Data.Fixed
import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Earth (r84)
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Transform
import Data.Geo.Jord.Vector3d
import Data.List (subsequences)

-- | Geodetic calculations assuming a spherical earth model.
--
-- No instance of class 'SGeodetics' for 'EcefPosition' is provided as the conversion requires
-- the mean earth radius. Conversion with 'ecefToNVectorSpherical' is therefore required beforehand.
class (Eq a) => SGeodetics a where
    -- | @angularDistance p1 p2 n@ computes the angle between the horizontal positions @p1@ and @p2@.
    -- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
    -- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
    angularDistance :: a -> a -> Maybe a -> Angle
    -- | @antipode p@ computes the antipodal horizontal position of @p@:
    -- the horizontal position on the surface of the Earth which is diametrically opposite to @p@.
    antipode :: a -> a
    -- | @destination p b d r@ computes the destination position from position @p@ having
    -- travelled the distance @d@ on the initial bearing @b@ (bearing will normally vary
    -- before destination is reached) and using the earth radius @r@.
    destination :: a -> Angle -> Length -> Length -> a
    destination p b d r
        | toMetres d == 0.0 = p
        | otherwise = _destination p b d r
    -- 'destination' using the mean radius of the WGS84 reference ellipsoid.
    destination84 :: a -> Angle -> Length -> a
    destination84 p b d = destination p b d r84
    -- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
    --  The final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
    -- Returns 180 if both horizontal positions are equals.
    finalBearing :: a -> a -> Angle
    finalBearing p1 p2 = normalise (initialBearing p2 p1) (decimalDegrees 180)
    -- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
    -- Returns 0 if both horizontal positions are equals.
    initialBearing :: a -> a -> Angle
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
    interpolate :: a -> a -> Double -> a
    interpolate p0 p1 f
        | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
        | f == 0 = p0
        | f == 1 = p1
        | otherwise = _interpolate p0 p1 f
    -- | @insideSurface p ps@ determines whether the @p@ is inside the polygon defined by the list of positions @ps@.
    -- The polygon is closed if needed (i.e. if @head ps /= last ps@).
    --
    -- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
    -- will sum to less than 360°, and exterior point angles will be small but non-zero.
    --
    -- Always returns 'False' if @ps@ does not at least defines a triangle.
    --
    insideSurface :: a -> [a] -> Bool
    insideSurface p ps
        | null ps = False
        | head ps == last ps = insideSurface p (init ps)
        | length ps < 3 = False
        | otherwise = _insideSurface p ps
    -- | @mean ps@ computes the mean geographic horitzontal position of @ps@, if it is defined.
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
    mean :: [a] -> Maybe a
    mean [] = Nothing
    mean [p] = Just p
    mean ps = _mean ps
    -- | @surfaceDistance p1 p2@ computes the surface distance (length of geodesic) between the positions @p1@ and @p2@.
    surfaceDistance :: a -> a -> Length -> Length
    surfaceDistance p1 p2 = arcLength (angularDistance p1 p2 Nothing)
    -- | 'surfaceDistance' using the mean radius of the WGS84 reference ellipsoid.
    surfaceDistance84 :: a -> a -> Length
    surfaceDistance84 p1 p2 = surfaceDistance p1 p2 r84
    -- | private (not exported): called by 'destination' if distance is > 0
    _destination :: a -> Angle -> Length -> Length -> a
    -- | private (not exported): called by 'insideSurface' after [a] has been checked.
    _insideSurface :: a -> [a] -> Bool
    -- | private (not exported): 'interpolate' after f has been checked.
    _interpolate :: a -> a -> Double -> a
    -- | private (not exported): called by 'mean' after [a] has been checked.
    _mean :: [a] -> Maybe a

-- | Spherical geodetics calculations on 'NVector's.
instance SGeodetics NVector where
    angularDistance (NVector v1) (NVector v2) (Just (NVector n)) = angularDistance' v1 v2 (Just n)
    angularDistance (NVector v1) (NVector v2) Nothing = angularDistance' v1 v2 Nothing
    antipode (NVector v) = NVector (vscale v (-1.0))
    initialBearing (NVector v1) (NVector v2) =
        normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
      where
        gc1 = vcross v1 v2 -- great circle through p1 & p2
        gc2 = vcross v1 (vec northPole) -- great circle through p1 & north pole
    _destination (NVector v) b d r = NVector (vadd (vscale v (cos' ta)) (vscale de (sin' ta)))
      where
        ed = vunit (vcross (vec northPole) v) -- east direction vector at v
        nd = vcross v ed -- north direction vector at v
        ta = central d r -- central angle
        de = vadd (vscale nd (cos' b)) (vscale ed (sin' b)) -- vunit vector in the direction of the azimuth
    _interpolate (NVector v0) (NVector v1) f = NVector (vunit (vadd v0 (vscale (vsub v1 v0) f)))
    _insideSurface (NVector v) vs =
        let aSum =
                foldl
                    (\a v' -> add a (uncurry angularDistance' v' (Just v)))
                    (decimalDegrees 0)
                    (egdes (map (vsub v) (fmap vec vs)))
         in abs (toDecimalDegrees aSum) > 180.0
    _mean vs =
        if null antipodals
            then Just (NVector (vunit (foldl vadd vzero vs')))
            else Nothing
      where
        vs' = fmap vec vs
        ts = filter (\l -> length l == 2) (subsequences vs')
        antipodals =
            filter (\t -> (realToFrac (vnorm (vadd (head t) (last t)) :: Double) :: Nano) == 0) ts

-- | Spherical geodetics calculations on 'LatLong's.
instance SGeodetics LatLong where
    angularDistance p1 p2 n = angularDistance (toNVector p1) (toNVector p2) (fmap toNVector n)
    antipode ll = fromNVector (antipode (toNVector ll)) zero
    initialBearing p1 p2 = initialBearing (toNVector p1) (toNVector p2)
    _destination p b d r = fromNVector (_destination (toNVector p) b d r) zero
    _interpolate p0 p1 f = fromNVector (_interpolate (toNVector p0) (toNVector p1) f) zero
    _insideSurface p ps = _insideSurface (toNVector p) (fmap toNVector ps)
    _mean ps = fmap (`fromNVector` zero) (_mean (fmap toNVector ps))

-- | Spherical geodetics calculations on 'NVector' 'AngularPosition's.
instance SGeodetics (AngularPosition NVector) where
    angularDistance (AngularPosition v1 _) (AngularPosition v2 _) n =
        angularDistance v1 v2 (fmap toNVector n)
    antipode p = AngularPosition (antipode (pos p)) (height p)
    initialBearing (AngularPosition v1 _) (AngularPosition v2 _) = initialBearing v1 v2
    _destination (AngularPosition v h) b d r = AngularPosition (_destination v b d r) h
    _interpolate (AngularPosition v0 h0) (AngularPosition v1 h1) f =
        AngularPosition (_interpolate v0 v1 f) (lrph h0 h1 f)
    _insideSurface p ps = _insideSurface (toNVector p) (fmap toNVector ps)
    _mean ps = fmap (`AngularPosition` zero) (_mean (fmap toNVector ps))

-- | Spherical geodetics calculations on 'LatLong' 'AngularPosition's.
instance SGeodetics (AngularPosition LatLong) where
    angularDistance p1 p2 n = angularDistance (toNVector p1) (toNVector p2) (fmap toNVector n)
    antipode (AngularPosition ll h) = fromNVector (antipode (toNVector ll)) h
    initialBearing p1 p2 = initialBearing (toNVector p1) (toNVector p2)
    _destination (AngularPosition ll h) b d r = fromNVector (_destination (toNVector ll) b d r) h
    _interpolate (AngularPosition ll0 h0) (AngularPosition ll1 h1) f =
        fromNVector (_interpolate (toNVector ll0) (toNVector ll1) f) (lrph h0 h1 f)
    _insideSurface p ps = _insideSurface (toNVector p) (fmap toNVector ps)
    _mean ps = fmap (`fromNVector` zero) (_mean (fmap toNVector ps))

-------------
-- private --
-------------
-- | Angle between the two given n-vectors.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
angularDistance' :: Vector3d -> Vector3d -> Maybe Vector3d -> Angle
angularDistance' v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . vdot (vcross v1 v2)) n
    sinO = sign * vnorm (vcross v1 v2)
    cosO = vdot v1 v2

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [Vector3d] -> [(Vector3d, Vector3d)]
egdes ps = zip ps (tail ps ++ [head ps])

lrph :: Length -> Length -> Double -> Length
lrph h0 h1 f = metres h
  where
    h0' = toMetres h0
    h1' = toMetres h1
    h = h0' + (h1' - h0') * f
