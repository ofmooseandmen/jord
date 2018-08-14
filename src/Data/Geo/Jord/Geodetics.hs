-- |
-- Module:      Data.Geo.Jord.Geodetics
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
module Data.Geo.Jord.Geodetics
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

-- | A circle on the __surface__ of the Earth which lies in a plane passing through
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
--
-- @
--     let p1 = decimalLatLongHeight 45.0 (-143.5) (metres 1500)
--     let p2 = decimalLatLongHeight 46.0 14.5 (metres 3000)
--     greatCircle p1 p2 -- heights are ignored, great circle are always at earth surface.
-- @
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
--
-- @
--     greatCircleBearing (readLatLong "283321N0290700W") (decimalDegrees 33.0)
-- @
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

-- | @crossTrackDistance p gc@ computes the signed distance from horizontal position @p@ to great circle @gc@.
-- Returns a negative 'Length' if position if left of great circle,
-- positive 'Length' if position if right of great circle; the orientation of the
-- great circle is therefore important:
--
-- @
--     let gc1 = greatCircle (decimalLatLong 51 0) (decimalLatLong 52 1)
--     let gc2 = greatCircle (decimalLatLong 52 1) (decimalLatLong 51 0)
--     crossTrackDistance p gc1 = (- crossTrackDistance p gc2)
--
--     let p = decimalLatLong 53.2611 (-0.7972)
--     let gc = greatCircleBearing (decimalLatLong 53.3206 (-1.7297)) (decimalDegrees 96.0)
--     crossTrackDistance p gc r84 -- -305.663 metres
-- @
crossTrackDistance :: (NTransform a) => a -> GreatCircle -> Length -> Length
crossTrackDistance p gc =
    arcLength (sub (angularDistance' (normal gc) (vector3d p) Nothing) (decimalDegrees 90))

-- | 'crossTrackDistance' using the mean radius of the WGS84 reference ellipsoid.
crossTrackDistance84 :: (NTransform a) => a -> GreatCircle -> Length
crossTrackDistance84 p gc = crossTrackDistance p gc r84

-- | @destination p b d r@ computes the destination position from position @p@ having
-- travelled the distance @d@ on the initial bearing (compass angle) @b@ (bearing will normally vary
-- before destination is reached) and using the earth radius @r@.
--
-- @
--     let p0 = ecefToNVector (ecefMetres 3812864.094 (-115142.863) 5121515.161) s84
--     let p1 = ecefMetres 3826406.4710518294 8900.536398998282 5112694.233184049
--     let p = destination p0 (decimalDegrees 96.0217) (metres 124800) r84
--     nvectorToEcef p s84 = p1
-- @
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
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
--
-- The final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
--
-- Returns 'Nothing' if both horizontal positions are equals.
finalBearing :: (Eq a, NTransform a) => a -> a -> Maybe Angle
finalBearing p1 p2 = fmap (\b -> normalise b (decimalDegrees 180)) (initialBearing p2 p1)

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
--
-- Returns 'Nothing' if both horizontal positions are equals.
initialBearing :: (Eq a, NTransform a) => a -> a -> Maybe Angle
initialBearing p1 p2
   | p1 == p2 = Nothing
   | otherwise = Just (normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360))
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
--     interpolate p0 p1 0.0 = p0
--     interpolate p0 p1 1.0 = p1
-- @
--
-- 'error's if @f < 0 || f > 1@
--
-- @
--     let p1 = latLongHeight (readLatLong "53°28'46''N 2°14'43''W") (metres 10000)
--     let p2 = latLongHeight (readLatLong "55°36'21''N 13°02'09''E") (metres 20000)
--     interpolate p1 p2 0.5 = decimalLatLongHeight 54.7835574 5.1949856 (metres 15000)
-- @
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

-- | @insideSurface p ps@ determines whether the @p@ is inside the polygon defined by the list of positions @ps@.
-- The polygon is closed if needed (i.e. if @head ps /= last ps@).
--
-- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
-- will sum to less than 360°, and exterior point angles will be small but non-zero.
--
-- Always returns 'False' if @ps@ does not at least defines a triangle.
--
-- @
--     let malmo = decimalLatLong 55.6050 13.0038
--     let ystad = decimalLatLong 55.4295 13.82
--     let lund = decimalLatLong 55.7047 13.1910
--     let helsingborg = decimalLatLong 56.0465 12.6945
--     let kristianstad = decimalLatLong 56.0294 14.1567
--     let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
--     let hoor = decimalLatLong 55.9295 13.5297
--     let hassleholm = decimalLatLong 56.1589 13.7668
--     insideSurface hoor polygon = True
--     insideSurface hassleholm polygon = False
-- @
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

-- | Computes the intersections between the two given 'GreatCircle's.
-- Two 'GreatCircle's intersect exactly twice unless there are equal (regardless of orientation),
-- in which case 'Nothing' is returned.
--
-- @
--     let gc1 = greatCircleBearing (decimalLatLong 51.885 0.235) (decimalDegrees 108.63)
--     let gc2 = greatCircleBearing (decimalLatLong 49.008 2.549) (decimalDegrees 32.72)
--     let (i1, i2) = fromJust (intersections gc1 gc2)
--     i1 = decimalLatLong 50.9017226 4.4942782
--     i2 = antipode i1
-- @
intersections :: (NTransform a) => GreatCircle -> GreatCircle -> Maybe (a, a)
intersections gc1 gc2
    | (vnorm i :: Double) == 0.0 = Nothing
    | otherwise
    , let ni = fromNVector (angular (vunit i) zero) = Just (ni, antipode ni)
  where
    i = vcross (normal gc1) (normal gc2)

-- | @mean ps@ computes the mean geographic horitzontal position of @ps@, if it is defined.
--
-- The geographic mean is not defined for antipodals position (since they
-- cancel each other).
--
-- Special conditions:
--
-- @
--     mean [] = Nothing
--     mean [p] = Just p
--     mean [p1, p2, p3] = Just circumcentre
--     mean [p1, .., antipode p1] = Nothing
-- @
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
