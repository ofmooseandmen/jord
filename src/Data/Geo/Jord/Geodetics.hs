{-# LANGUAGE FlexibleInstances #-}

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
    , IsGreatCircle(..)
    , gcPos
    , gcBearing
    -- * The 'GreatArc' type
    , GreatArc
    , IsGreatArc(..)
    , gaStart
    , gaEnd
    -- * Calculations
    , alongTrackDistance
    , alongTrackDistance84
    , angularDistance
    , antipode
    , crossTrackDistance
    , crossTrackDistance84
    , destination
    , destination84
    , finalBearing
    , initialBearing
    , interpolate
    , intersection
    , intersections
    , isBetween
    , isInsideSurface
    , mean
    , surfaceDistance
    , surfaceDistance84
    ) where

import Control.Monad.Fail
import Data.Fixed
import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Earth (r84)
import Data.Geo.Jord.Internal (nvec, sad)
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Transformation
import Data.Geo.Jord.Vector3d
import Data.List (subsequences)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Prelude hiding (fail)

-- | A circle on the __surface__ of the Earth which lies in a plane passing through
-- the Earth's centre. Every two distinct and non-antipodal points on the surface
-- of the Earth define a Great Circle.
--
-- It is internally represented as its normal vector - i.e. the normal vector
-- to the plane containing the great circle.
--
-- see 'IsGreatCircle'.
data GreatCircle = GreatCircle
    { gcNormal :: Vector3d -- ^ normal vector to the plane containing the great circle
    , gcPos :: NVector -- ^ position (/n/-vector) on the great circle
    , gcBearing :: Angle -- ^ bearing from 'gcPos'.
    } deriving (Eq, Show)

-- | Class for data from which a 'GreatCircle' can be computed.
class IsGreatCircle a where
    greatCircle :: a -> GreatCircle -- ^ 'GreatCircle' from @a@, if 'greateCircleE' returns a 'Left', this function 'error's.
    greatCircle a = fromMaybe (error "Could not make a Great Circle") (greatCircleF a)
    greatCircleE :: a -> Either String GreatCircle -- ^ 'GreatCircle' from @a@, A 'Left' indicates an error.
    greatCircleF :: (MonadFail m) => a -> m GreatCircle -- ^ 'GreatCircle' from @a@, if 'greateCircleE' returns a 'Left', this function 'fail's.
    greatCircleF a =
        case e of
            Left err -> fail err
            Right gc -> return gc
      where
        e = greatCircleE a

-- | A closed segment of 'GreatCircle'. It represent the shortest path on the __surface__ of the Earth
--  from between the two positions.
--
-- see 'IsGreatArc'.
data GreatArc = GreatArc
    { gaNormal :: Vector3d -- ^ normal vector to the plane containing the great circle
    , gaStart :: NVector -- ^ start position (/n/-vector) of the great arc
    , gaEnd :: NVector -- ^ end position (/n/-vector) of the great arc
    } deriving (Eq, Show)

-- | Class for data from which a 'GreatArc' can be computed.
class IsGreatArc a where
    greatArc :: a -> GreatArc -- ^ 'GreatCircle' from @a@, if 'greatArcE' returns a 'Left', this function 'error's.
    greatArc a = fromMaybe (error "Could not make a Great Arc") (greatArcF a)
    greatArcE :: a -> Either String GreatArc -- ^ 'GreatArc' from @a@, A 'Left' indicates an error.
    greatArcF :: (MonadFail m) => a -> m GreatArc -- ^ 'GreatArc' from @a@, if 'greatArcE' returns a 'Left', this function 'fail's.
    greatArcF a =
        case e of
            Left err -> fail err
            Right ga -> return ga
      where
        e = greatArcE a

-- | 'GreatCircle' passing by both given positions'. A 'Left' indicates that given positions are
-- equal or antipodal.
--
-- @
--     let p1 = decimalLatLongHeight 45.0 (-143.5) (metres 1500)
--     let p2 = decimalLatLongHeight 46.0 14.5 (metres 3000)
--     greatCircle (p1, p2) -- heights are ignored, great circle are always at earth surface.
-- @
instance NTransform a => IsGreatCircle (a, a) where
    greatCircleE (p1, p2)
        | v1 == v2 = Left "Invalid Great Circle: positions are equal"
        | (realToFrac (vnorm (vadd v1 v2)) :: Nano) == 0 =
            Left "Invalid Great Circle: positions are antipodal"
        | isNothing b = Left "Invalid Great Circle: positions are equal"
        | otherwise = Right (GreatCircle (vcross v1 v2) nv1 (fromJust b))
      where
        nv1 = pos . toNVector $ p1
        v1 = vec nv1
        nv2 = pos . toNVector $ p2
        v2 = vec nv2
        b = initialBearing nv1 nv2

-- | 'GreatCircle' passing by the given position and heading on given bearing.
--
-- @
--     greatCircle (readLatLong "283321N0290700W", decimalDegrees 33.0)
-- @
instance NTransform a => IsGreatCircle (a, Angle) where
    greatCircleE (p, b) = Right (GreatCircle (vsub n' e') nv b)
      where
        nv = pos . toNVector $ p
        v = nvec nv
        e = vcross (vec northPole) v -- easting
        n = vcross v e -- northing
        e' = vscale e (cos' b / vnorm e)
        n' = vscale n (sin' b / vnorm n)

-- | 'GreatCircle' from given 'GreatArc'.
instance IsGreatCircle GreatArc where
    greatCircleE (GreatArc n s e) =
        case initialBearing s e of
            Nothing -> Left "Could not computed initial bearing"
            (Just b) -> Right (GreatCircle n s b)

-- | 'GreatArc' passing by both given positions'. A 'Left' indicates that given positions are
-- equal or antipodal.
--
-- @
--     let p1 = decimalLatLongHeight 45.0 (-143.5) (metres 1500)
--     let p2 = decimalLatLongHeight 46.0 14.5 (metres 3000)
--     greatArc (p1, p2) -- heights are ignored, great arc are always at earth surface.
-- @
instance NTransform a => IsGreatArc (a, a) where
    greatArcE ps@(p1, p2) =
        case greatCircleE ps of
            Left e -> Left e
            Right gcv ->
                Right (GreatArc (gcNormal gcv) (pos . toNVector $ p1) (pos . toNVector $ p2))

-- | @alongTrackDistance p ga r@ how far position @p@ is along a path described
-- by great arc @ga@: if a perpendicular is drawn from @p@  to the great arc, the
-- along-track distance is the signed distance from the start point to where the
-- perpendicular crosses the path.
--
-- @
--     let p = decimalLatLong 53.2611 (-0.7972)
--     let ga = greatArc (decimalLatLong 53.3206 (-1.7297)) (decimalLatLong 53.1887 0.1334)
--     alongTrackDistance p ga r84 -- 62.3315757 kilometres
-- @
alongTrackDistance :: (NTransform a) => a -> GreatArc -> Length -> Length
alongTrackDistance p (GreatArc n s _) =
    arcLength (sad' (nvec s) (vcross (vcross n (nvec p)) n) (Just n))

-- | 'alongTrackDistance' using the mean radius of the WGS84 reference ellipsoid.
alongTrackDistance84 :: (NTransform a) => a -> GreatArc -> Length
alongTrackDistance84 p ga = alongTrackDistance p ga r84

-- | @angularDistance p1 p2 n@ computes the angle between the horizontal positions @p1@ and @p2@.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
angularDistance :: (NTransform a) => a -> a -> Maybe a -> Angle
angularDistance p1 p2 n = sad' v1 v2 vn
  where
    v1 = nvec p1
    v2 = nvec p2
    vn = fmap nvec n

-- | @antipode p@ computes the antipodal horizontal position of @p@:
-- the horizontal position on the surface of the Earth which is diametrically opposite to @p@.
antipode :: (NTransform a) => a -> a
antipode p = fromNVector (angular (vscale (nvec nv) (-1.0)) h)
  where
    (AngularPosition nv h) = toNVector p

-- | @crossTrackDistance p gc r@ computes the signed distance from horizontal position @p@ to great circle @gc@.
-- Returns a negative 'Length' if position if left of great circle,
-- positive 'Length' if position if right of great circle; the orientation of the
-- great circle is therefore important:
--
-- @
--     let gc1 = greatCircle (decimalLatLong 51 0) (decimalLatLong 52 1)
--     let gc2 = greatCircle (decimalLatLong 52 1) (decimalLatLong 51 0)
--     crossTrackDistance p gc1 r84 = (- crossTrackDistance p gc2 r84)
--
--     let p = decimalLatLong 53.2611 (-0.7972)
--     let gc = greatCircleBearing (decimalLatLong 53.3206 (-1.7297)) (decimalDegrees 96.0)
--     crossTrackDistance p gc r84 -- -305.663 metres
-- @
crossTrackDistance :: (NTransform a) => a -> GreatCircle -> Length -> Length
crossTrackDistance p (GreatCircle n _ _) =
    arcLength (sub (sad' n (nvec p) Nothing) (decimalDegrees 90))

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
    | otherwise = Just (normalise (sad' gc1 gc2 (Just v1)) (decimalDegrees 360))
  where
    v1 = nvec p1
    v2 = nvec p2
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

-- | Computes the intersection between the two given 'GreatArc's.
--
-- see also 'intersections'
--
-- @
--     let spd = kilometresPerHour 1000
--     let t1 = Track (decimalLatLong 51.885 0.235) (decimalDegrees 108.63) spd
--     let t2 = Track (decimalLatLong 49.008 2.549) (decimalDegrees 32.72) spd
--     let oneHour = hours 1
--     let ga1 = greatArc (t1, oneHour)
--     let ga2 = greatArc (t2, oneHour)
--     intersection ga1 ga2 = Just (decimalLatLong 50.9017225 4.494278333333333)
-- @
intersection :: (NTransform a) => GreatArc -> GreatArc -> Maybe a
intersection ga@(GreatArc n1 _ _) (GreatArc n2 _ _) =
    case intersections' n1 n2 of
        Nothing -> Nothing
        (Just (i1, i2))
            | isBetween i1 ga -> Just i1
            | isBetween i2 ga -> Just i2
            | otherwise -> Nothing

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
intersections (GreatCircle n1 _ _) (GreatCircle n2 _ _) = intersections' n1 n2

-- | @isBetween p ga@ determines whether position @p@ is between start and end points
-- of great arc @ga@.
-- If @p@ is not on the great arc, returns whether @p@ is within the area bound
-- by perpendiculars to the great arc at each point (in the same hemisphere).
--
isBetween :: (NTransform a) => a -> GreatArc -> Bool
isBetween p (GreatArc _ s e) = between && hemisphere
  where
    v0 = nvec p
    v1 = nvec s
    v2 = nvec e
    v10 = vsub v0 v1
    v12 = vsub v2 v1
    v20 = vsub v0 v2
    v21 = vsub v1 v2
    e1 = vdot v10 v12 -- p is on e side of s
    e2 = vdot v20 v21 -- p is on s side of e
    between = e1 >= 0 && e2 >= 0
    hemisphere = vdot v0 v1 >= 0 && vdot v0 v2 >= 0

-- | @isInsideSurface p ps@ determines whether the @p@ is inside the polygon defined by the list of positions @ps@.
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
--     isInsideSurface hoor polygon = True
--     isInsideSurface hassleholm polygon = False
-- @
isInsideSurface :: (Eq a, NTransform a) => a -> [a] -> Bool
isInsideSurface p ps
    | null ps = False
    | head ps == last ps = isInsideSurface p (init ps)
    | length ps < 3 = False
    | otherwise =
        let aSum =
                foldl
                    (\a v' -> add a (uncurry sad' v' (Just v)))
                    (decimalDegrees 0)
                    (egdes (map (vsub v) vs))
         in abs (toDecimalDegrees aSum) > 180.0
  where
    v = nvec p
    vs = fmap nvec ps

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
    vs = fmap nvec ps
    ts = filter (\l -> length l == 2) (subsequences vs)
    antipodals =
        filter (\t -> (realToFrac (vnorm (vadd (head t) (last t)) :: Double) :: Nano) == 0) ts

-- | @surfaceDistance p1 p2@ computes the surface distance (length of geodesic) between the positions @p1@ and @p2@.
surfaceDistance :: (NTransform a) => a -> a -> Length -> Length
surfaceDistance p1 p2 = arcLength (angularDistance p1 p2 Nothing)

-- | 'surfaceDistance' using the mean radius of the WGS84 reference ellipsoid.
surfaceDistance84 :: (NTransform a) => a -> a -> Length
surfaceDistance84 p1 p2 = surfaceDistance p1 p2 r84

-- | Signed angular distance - see 'sad'.
sad' :: Vector3d -> Vector3d -> Maybe Vector3d -> Angle
sad' v1 v2 n = radians (sad v1 v2 n)

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [Vector3d] -> [(Vector3d, Vector3d)]
egdes ps = zip ps (tail ps ++ [head ps])

lrph :: Length -> Length -> Double -> Length
lrph h0 h1 f = metres h
  where
    h0' = toMetres h0
    h1' = toMetres h1
    h = h0' + (h1' - h0') * f

angular :: Vector3d -> Length -> AngularPosition NVector
angular v = nvectorHeight (nvector (vx v) (vy v) (vz v))

intersections' :: (NTransform a) => Vector3d -> Vector3d -> Maybe (a, a)
intersections' n1 n2
    | (vnorm i :: Double) == 0.0 = Nothing
    | otherwise
    , let ni = fromNVector (angular (vunit i) zero) = Just (ni, antipode ni)
  where
    i = vcross n1 n2
