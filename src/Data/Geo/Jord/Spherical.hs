{-# LANGUAGE FlexibleInstances #-}

--
-- TODO: doc
--
module Data.Geo.Jord.Spherical
    ( SGeodetics(angularDistance, antipode, finalBearing, initialBearing,
           interpolate, insideSurface, mean, surfaceDistance)
    , northPole
    , southPole
    ) where

import Data.Fixed
import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Transform
import Data.List (subsequences)

-- | Horizontal position of the North Pole.
northPole :: NVector
northPole = NVector 0.0 0.0 1.0

-- | Horizontal position of the South Pole.
southPole :: NVector
southPole = NVector 0.0 0.0 (-1.0)

-- | Geodetics calculations assuming a spherical earth model.
--
-- No instance of class 'SGeodetics' for 'EcefPosition' is provided as the conversion requires
-- the mean earth radius. Conversion with 'toEcef' is therefore required beforehand.
class (Eq a) => SGeodetics a where
    -- | @angularDistance p1 p2 n@ computes the angle between the horizontal positions @p1@ and @p2@.
    -- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
    -- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
    angularDistance :: a -> a -> Maybe a -> Angle
    -- | @antipode p@ computes the antipodal horizontal position of @p@:
    -- the horizontal position on the surface of the Earth which is diametrically opposite to @p@.
    antipode :: a -> a
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
    -- private (not exported)
    _insideSurface :: a -> [a] -> Bool
    _interpolate :: a -> a -> Double -> a
    _mean :: [a] -> Maybe a

-- | Spherical geodetics calculations on 'NVector's.
instance SGeodetics NVector where
    angularDistance = angularDistance'
    antipode v = scale v (-1.0)
    initialBearing v1 v2 = normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
      where
        gc1 = cross v1 v2 -- great circle through p1 & p2
        gc2 = cross v1 northPole -- great circle through p1 & north pole
    _interpolate v0 v1 f = unit (add v0 (scale (sub v1 v0) f))
    _insideSurface v vs =
        let aSum =
                foldl
                    (\a v' -> add a (uncurry angularDistance' v' (Just v)))
                    (decimalDegrees 0)
                    (egdes (map (sub v) vs))
         in abs (toDecimalDegrees aSum) > 180.0
    _mean vs =
        if null antipodals
            then Just (unit (foldl add zero vs))
            else Nothing
      where
        ts = filter (\l -> length l == 2) (subsequences vs)
        antipodals =
            filter (\t -> (realToFrac (norm (add (head t) (last t)) :: Double) :: Nano) == 0) ts

-- | Spherical geodetics calculations on 'LatLong's.
instance SGeodetics LatLong where
    angularDistance p1 p2 n = angularDistance (toNVector p1) (toNVector p2) (fmap toNVector n)
    antipode ll = fromNVector (antipode (toNVector ll)) 0.0
    initialBearing p1 p2 = initialBearing (toNVector p1) (toNVector p2)
    _interpolate p0 p1 f = fromNVector (_interpolate (toNVector p0) (toNVector p1) f) 0.0
    _insideSurface p ps = _insideSurface (toNVector p) (fmap toNVector ps)
    _mean ps = fmap (`fromNVector` 0.0) (_mean (fmap toNVector ps))

-- | Spherical geodetics calculations on 'NVector' 'AngularPosition's.
instance SGeodetics (AngularPosition NVector) where
    angularDistance (AngularPosition v1 _) (AngularPosition v2 _) n =
        angularDistance v1 v2 (fmap toNVector n)
    antipode p = AngularPosition (antipode (pos p)) (height p)
    initialBearing (AngularPosition v1 _) (AngularPosition v2 _) = initialBearing v1 v2
    _interpolate (AngularPosition v0 h0) (AngularPosition v1 h1) f =
        AngularPosition (_interpolate v0 v1 f) h
      where
        h = h0 + (h1 - h0) * f
    _insideSurface p ps = _insideSurface (toNVector p) (fmap toNVector ps)
    _mean ps = fmap (`AngularPosition` 0.0) (_mean (fmap toNVector ps))

-- | Spherical geodetics calculations on 'LatLong' 'AngularPosition's.
instance SGeodetics (AngularPosition LatLong) where
    angularDistance p1 p2 n = angularDistance (toNVector p1) (toNVector p2) (fmap toNVector n)
    antipode (AngularPosition ll h) = fromNVector (antipode (toNVector ll)) h
    initialBearing p1 p2 = initialBearing (toNVector p1) (toNVector p2)
    _interpolate (AngularPosition ll0 h0) (AngularPosition ll1 h1) f =
        fromNVector (_interpolate (toNVector ll0) (toNVector ll1) f) h
      where
        h = h0 + (h1 - h0) * f
    _insideSurface p ps = _insideSurface (toNVector p) (fmap toNVector ps)
    _mean ps = fmap (`fromNVector` 0.0) (_mean (fmap toNVector ps))

-------------
-- private --
-------------
-- | Angle between the two given n-vectors.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
angularDistance' :: NVector -> NVector -> Maybe NVector -> Angle
angularDistance' v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [NVector] -> [(NVector, NVector)]
egdes ps = zip ps (tail ps ++ [head ps])
