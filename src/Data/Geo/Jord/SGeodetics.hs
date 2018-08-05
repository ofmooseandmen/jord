{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Data.Geo.Jord.SGeodetics
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
module Data.Geo.Jord.SGeodetics
    (
    -- * The 'GreatCircle' type
      GreatCircle
    -- * Smart constructors
    , greatCircle
    , greatCircleE
    , greatCircleF
    , greatCircleBearing
    -- * Calculations
    , angularDistance
    , antipode
    , crossTrackDistance
    , crossTrackDistance84
    , destination
    , destination84
    , finalBearing
    , initialBearing
    , interpolate
    , intersections
    , insideSurface
    , mean
    , surfaceDistance
    , surfaceDistance84
    ) where

import Control.Monad.Fail
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
import Data.Maybe (fromMaybe)
import Prelude hiding (fail)

-- | A circle on the _surface_ of the Earth which lies in a plane passing through
-- the Earth's centre. Every two distinct and non-antipodal points on the surface
-- of the Earth define a Great Circle.
--
-- It is internally represented as its normal vector - i.e. the normal vector
-- to the plane containing the great circle.
--
-- See 'greatCircle', 'greatCircleE', 'greatCircleF' or 'greatCircleBearing' constructors.
--
data GreatCircle = GreatCircle
    { normal :: Vector3d
    , dscr :: String
    } deriving (Eq)

instance Show GreatCircle where
    show = dscr

-- | 'GreatCircle' passing by both given positions. 'error's if given positions are
-- equal or antipodal.
greatCircle :: (NTransform a, Show a) => a -> a -> GreatCircle
greatCircle p1 p2 =
    fromMaybe
        (error (show p1 ++ " and " ++ show p2 ++ " do not define a unique Great Circle"))
        (greatCircleF p1 p2)

-- | 'GreatCircle' passing by both given positions. A 'Left' indicates that given positions are
-- equal or antipodal.
greatCircleE :: (NTransform a) => a -> a -> Either String GreatCircle
greatCircleE p1 p2
    | v1 == v2 = Left "Invalid Great Circle: positions are equal"
    | (realToFrac (vnorm (vadd v1 v2)) :: Nano) == 0 =
        Left "Invalid Great Circle: positions are antipodal"
    | otherwise =
        Right (GreatCircle (vcross v1 v2) ("passing by " ++ show (ll p1) ++ " & " ++ show (ll p2)))
  where
    v1 = vector3d p1
    v2 = vector3d p2

-- | 'GreatCircle' passing by both given positions. 'fail's if given positions are
-- equal or antipodal.
greatCircleF :: (NTransform a, MonadFail m) => a -> a -> m GreatCircle
greatCircleF p1 p2 =
    case e of
        Left err -> fail err
        Right gc -> return gc
  where
    e = greatCircleE p1 p2

-- | 'GreatCircle' passing by the given position and heading on given bearing.
greatCircleBearing :: (NTransform a) => a -> Angle -> GreatCircle
greatCircleBearing p b =
    GreatCircle (vsub n' e') ("passing by " ++ show (ll p) ++ " heading on " ++ show b)
  where
    v = vector3d p
    e = vcross (vec northPole) v -- easting
    n = vcross v e -- northing
    e' = vscale e (cos' b / vnorm e)
    n' = vscale n (sin' b / vnorm n)

-- | @angularDistance p1 p2 n@ computes the angle between the horizontal positions @p1@ and @p2@.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
angularDistance :: (NTransform a) => a -> a -> Maybe a -> Angle
angularDistance p1 p2 n = angularDistance' v1 v2 vn
  where
    v1 = vector3d p1
    v2 = vector3d p2
    vn = fmap vector3d n

-- | @antipode p@ computes the antipodal horizontal position of @p@:
-- the horizontal position on the surface of the Earth which is diametrically opposite to @p@.
antipode :: (NTransform a) => a -> a
antipode p = fromNVector (angular (vscale (vector3d nv) (-1.0)) h)
  where
    (AngularPosition nv h) = toNVector p

-- | @crossTrackDistance p gc@ computes the signed distance horizontal position @p@ to great circle @gc@.
-- Returns a negative 'Length' if position if left of great circle,
-- positive 'Length' if position if right of great circle; the orientation of the
-- great circle is therefore important:
--
-- @
--     let gc1 = greatCircle (decimalLatLong 51 0) (decimalLatLong 52 1)
--     let gc2 = greatCircle (decimalLatLong 52 1) (decimalLatLong 51 0)
--     crossTrackDistance p gc1 == (- crossTrackDistance p gc2)
-- @
crossTrackDistance :: (NTransform a) => a -> GreatCircle -> Length -> Length
crossTrackDistance p gc =
    arcLength (sub (angularDistance' (normal gc) (vector3d p) Nothing) (decimalDegrees 90))

-- | 'crossTrackDistance' using the mean radius of the WGS84 reference ellipsoid.
crossTrackDistance84 :: (NTransform a) => a -> GreatCircle -> Length
crossTrackDistance84 p gc = crossTrackDistance p gc r84

-- | @destination p b d r@ computes the destination position from position @p@ having
-- travelled the distance @d@ on the initial bearing @b@ (bearing will normally vary
-- before destination is reached) and using the earth radius @r@.
destination :: (NTransform a) => a -> Angle -> Length -> Length -> a
destination p b d r
    | toMetres d == 0.0 = p
    | otherwise = fromNVector (angular vd h)
  where
    (AngularPosition nv h) = toNVector p
    v = vec nv
    ed = vunit (vcross (vec northPole) v) -- east direction vector at v
    nd = vcross v ed -- north direction vector at v
    ta = central d r -- central angle
    de = vadd (vscale nd (cos' b)) (vscale ed (sin' b)) -- vunit vector in the direction of the azimuth
    vd = vadd (vscale v (cos' ta)) (vscale de (sin' ta))

-- | 'destination' using the mean radius of the WGS84 reference ellipsoid.
destination84 :: (NTransform a) => a -> Angle -> Length -> a
destination84 p b d = destination p b d r84

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
--  The final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
-- Returns 180 if both horizontal positions are equals.
finalBearing :: (NTransform a) => a -> a -> Angle
finalBearing p1 p2 = normalise (initialBearing p2 p1) (decimalDegrees 180)

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
-- Returns 0 if both horizontal positions are equals.
initialBearing :: (NTransform a) => a -> a -> Angle
initialBearing p1 p2 = normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
  where
    v1 = vector3d p1
    v2 = vector3d p2
    gc1 = vcross v1 v2 -- great circle through p1 & p2
    gc2 = vcross v1 (vec northPole) -- great circle through p1 & north pole

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
interpolate :: (NTransform a) => a -> a -> Double -> a
interpolate p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = fromNVector (angular iv ih)
  where
    (AngularPosition nv0 h0) = toNVector p0
    (AngularPosition nv1 h1) = toNVector p1
    v0 = vec nv0
    v1 = vec nv1
    iv = vunit (vadd v0 (vscale (vsub v1 v0) f))
    ih = lrph h0 h1 f

-- | Computes the intersections between the two given 'GreatCircle's.
-- Two 'GreatCircle's intersect exactly twice unless there are equal (regardless of orientation),
-- in which case 'Nothing' is returned.
intersections :: (NTransform a) => GreatCircle -> GreatCircle -> Maybe (a, a)
intersections gc1 gc2
    | (vnorm i :: Double) == 0.0 = Nothing
    | otherwise
    , let ni = fromNVector (angular (vunit i) zero) = Just (ni, antipode ni)
  where
    i = vcross (normal gc1) (normal gc2)

-- | @insideSurface p ps@ determines whether the @p@ is inside the polygon defined by the list of positions @ps@.
-- The polygon is closed if needed (i.e. if @head ps /= last ps@).
--
-- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
-- will sum to less than 360°, and exterior point angles will be small but non-zero.
--
-- Always returns 'False' if @ps@ does not at least defines a triangle.
--
insideSurface :: (Eq a, NTransform a) => a -> [a] -> Bool
insideSurface p ps
    | null ps = False
    | head ps == last ps = insideSurface p (init ps)
    | length ps < 3 = False
    | otherwise =
        let aSum =
                foldl
                    (\a v' -> add a (uncurry angularDistance' v' (Just v)))
                    (decimalDegrees 0)
                    (egdes (map (vsub v) vs))
         in abs (toDecimalDegrees aSum) > 180.0
  where
    v = vector3d p
    vs = fmap vector3d ps

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
mean :: (NTransform a) => [a] -> Maybe a
mean [] = Nothing
mean [p] = Just p
mean ps =
    if null antipodals
        then Just (fromNVector (angular (vunit (foldl vadd vzero vs)) zero))
        else Nothing
  where
    vs = fmap vector3d ps
    ts = filter (\l -> length l == 2) (subsequences vs)
    antipodals =
        filter (\t -> (realToFrac (vnorm (vadd (head t) (last t)) :: Double) :: Nano) == 0) ts

-- | @surfaceDistance p1 p2@ computes the surface distance (length of geodesic) between the positions @p1@ and @p2@.
surfaceDistance :: (NTransform a) => a -> a -> Length -> Length
surfaceDistance p1 p2 = arcLength (angularDistance p1 p2 Nothing)

-- | 'surfaceDistance' using the mean radius of the WGS84 reference ellipsoid.
surfaceDistance84 :: (NTransform a) => a -> a -> Length
surfaceDistance84 p1 p2 = surfaceDistance p1 p2 r84

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

vector3d :: (NTransform a) => a -> Vector3d
vector3d = vec . pos . toNVector

angular :: Vector3d -> Length -> AngularPosition NVector
angular v = nvectorHeight (nvector (vx v) (vy v) (vz v))

ll :: (NTransform a) => a -> LatLong
ll = nvectorToLatLong . pos . toNVector
