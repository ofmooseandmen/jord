-- |
-- Module:      Data.Geo.Jord.GreatCircle
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geographical Position calculations on great circles, i.e. using a __sphere__ to represent
-- the celestial body that positions refer to..
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Point_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.GreatCircle
    (
    -- * The 'GreatCircle' type
      GreatCircle
    , greatCircle
    , greatCircleThrough
    , greatCircleHeadingOn
    -- * Calculations
    , alongTrackDistance
    , alongTrackDistance'
    , angularDistance
    , crossTrackDistance
    , crossTrackDistance'
    , interpolate
    , intersection
    , intersections
    , isBetween
    , isInsideSurface
    , mean
    ) where

-- TODO: add the following
-- see: https://github.com/chrisveness/geodesy/blob/master/latlon-nvector-spherical.js
-- intermediatePointOnChordTo
-- nearestPointOnSegment
-- triangulate
-- trilaterate
import Data.Fixed (Nano)
import Data.List (subsequences)

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Bodies
import Data.Geo.Jord.Internal
import Data.Geo.Jord.Length
import Data.Geo.Jord.Position
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Vector3d

-- | A circle on the __surface__ of a __sphere__ which lies in a plane
-- passing through the sphere centre. Every two distinct and non-antipodal points
-- define a Great Circle.
--
-- It is internally represented as its normal vector - i.e. the normal vector
-- to the plane containing the great circle.
--
data GreatCircle a =
    GreatCircle !Vector3d !a String
    deriving (Eq)

instance (Model a) => Show (GreatCircle a) where
    show (GreatCircle _ _ s) = s

-- | 'GreatCircle' from 'MinorArc'. -- TODO great circle description
greatCircle :: (Spherical a) => MinorArc a -> GreatCircle a
greatCircle (MinorArc n s _) = GreatCircle n (model s) ""

-- | 'GreatCircle' passing by both given positions.
-- A 'Left' indicates that given positions are equal.
--
-- @
--     let p1 = latLongHeightPos 45.0 (-143.5) (metres 1500) S84
--     let p2 = latLongHeightPos 46.0 14.5 (metres 3000) S84
--     greatCircleThrough p1 p2 -- heights are ignored, great circle is always at surface.
-- @
greatCircleThrough :: (Spherical a) => Position a -> Position a -> Either String (GreatCircle a)
greatCircleThrough p1 p2
    | p1 == p2 = Left "Invalid Great Circle: positions are equal"
    | otherwise = Right (GreatCircle (normal' p1 p2) (model p1) "")

-- | 'GreatCircle' passing by the given position and heading on given bearing.
--
-- @
--     let p = latLongPos 45.0 (-143.5) S84
--     let b = decimalDegrees 33.0
--     greatCircleHeadingOn p b
-- @
greatCircleHeadingOn :: (Spherical a) => Position a -> Angle -> GreatCircle a
greatCircleHeadingOn p b = GreatCircle (vsub n' e') (model p) ""
  where
    v = nvec p
    e = vcross nvNorthPole v -- easting
    n = vcross v e -- northing
    e' = vscale e (cos' b / vnorm e)
    n' = vscale n (sin' b / vnorm n)

-- | Minor arc of a great circle between two positions.
data MinorArc a = MinorArc !Vector3d (Position a) (Position a)
    deriving (Eq)

instance (Model a) => Show (MinorArc a) where
  show (MinorArc _ s e) = "Minor Arc { from: " ++ (show s) ++ ", to: " ++ (show e) ++ "}"


-- | @alongTrackDistance p a@ computes how far Position @p@ is along a path described
-- by the minor arc @a@: if a perpendicular is drawn from @p@  to the path, the
-- along-track distance is the signed distance from the start point to where the
-- perpendicular crosses the path.
--
-- @ TODO review
--     let p = s84Pos 53.2611 (-0.7972) zero
--     let g = inverse (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
--     alongTrackDistance p g
--     -- 62.3315757 kilometres
-- @
alongTrackDistance :: (Spherical a) => Position a -> MinorArc a -> Length
alongTrackDistance p (MinorArc n s _) = alongTrackDistance'' p s n

alongTrackDistance' :: (Spherical a) => Position a -> Position a -> Angle -> Length
alongTrackDistance' p s b = alongTrackDistance'' p s n
  where
    (GreatCircle n _ _) = greatCircleHeadingOn s b

alongTrackDistance'' :: (Spherical a) => Position a -> Position a -> Vector3d -> Length
alongTrackDistance'' p s n = arcLength a (radius s)
  where
    a = signedAngle (nvec s) (vcross (vcross n (nvec p)) n) (Just n)

-- | @angularDistance p1 p2 n@ computes the angle between the horizontal Points @p1@ and @p2@.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
angularDistance :: (Spherical a) => Position a -> Position a -> Maybe (Position a) -> Angle
angularDistance p1 p2 n = signedAngle v1 v2 vn
  where
    v1 = nvec p1
    v2 = nvec p2
    vn = fmap nvec n

-- | @crossTrackDistance p gc@ computes the signed distance from horizontal Position @p@ to great circle @gc@.
-- Returns a negative 'Length' if Position if left of great circle,
-- positive 'Length' if Position if right of great circle; the orientation of the
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
crossTrackDistance :: (Spherical a) => Position a -> GreatCircle a -> Length
crossTrackDistance p (GreatCircle n _ _) = arcLength (sub a (decimalDegrees 90)) (radius p)
  where
    a = radians (angleRadians n (nvec p))

crossTrackDistance' :: (Spherical a) => Position a -> Position a -> Angle -> Length
crossTrackDistance' p s b = crossTrackDistance p gc
  where
    gc = greatCircleHeadingOn s b

-- | @interpolate p0 p1 f# computes the horizontal Position at fraction @f@ between the @p0@ and @p1@.
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
interpolate :: (Spherical a) => Position a -> Position a -> Double -> Position a
interpolate p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = nvh iv ih (model p0)
  where
    nv0 = nvec p0
    h0 = height p0
    nv1 = nvec p1
    h1 = height p1
    iv = vunit (vadd nv0 (vscale (vsub nv1 nv0) f))
    ih = lrph h0 h1 f

-- | Computes the intersection between the two given minor arcs of great circle.
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
intersection :: (Spherical a) => MinorArc a -> MinorArc a -> Maybe (Position a)
intersection a1@(MinorArc n1 s1 _) a2@(MinorArc n2 _ _) =
    case intersections' n1 n2 (model s1) of
        Nothing -> Nothing
        (Just (i1, i2))
            | isBetween i1 a1 && isBetween i1 a2 -> Just i1
            | isBetween i2 a1 && isBetween i2 a2 -> Just i2
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
intersections :: (Spherical a) => GreatCircle a -> GreatCircle a -> Maybe (Position a, Position a)
intersections (GreatCircle n1 m _) (GreatCircle n2 _ _) = intersections' n1 n2 m

-- | @isBetween p a@ determines whether position @p@ is within the minor arc
-- of great circle @a@.
--
-- If @p@ is not on the arc, returns whether @p@ is within the area bound
-- by perpendiculars to the arc at each point (in the same hemisphere).
--
isBetween :: (Spherical a) => Position a -> MinorArc a -> Bool
isBetween p (MinorArc _ s e) = between && hemisphere
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

-- | @isInsideSurface p ps@ determines whether the @p@ is inside the polygon defined by the list of Points @ps@.
-- The polygon can be opened or closed (i.e. if @head ps /= last ps@).
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
isInsideSurface :: (Spherical a) => Position a -> [Position a] -> Bool
isInsideSurface p ps
    | null ps = False
    | head ps == last ps = isInsideSurface p (init ps)
    | length ps < 3 = False
    | otherwise =
        let aSum =
                foldl
                    (\a v' -> add a (uncurry signedAngle v' (Just v)))
                    (decimalDegrees 0)
                    (egdes (map (vsub v) vs))
         in abs (toDecimalDegrees aSum) > 180.0
  where
    v = nvec p
    vs = fmap nvec ps

-- | @mean ps@ computes the mean geographic horitzontal Position of @ps@, if it is defined.
--
-- The geographic mean is not defined for antipodals Position (since they
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
mean :: (Spherical a) => [Position a] -> Maybe (Position a)
mean [] = Nothing
mean [p] = Just p
mean ps =
    if null antipodals
        then Just (nvh nv zero (model . head $ ps))
        else Nothing
  where
    vs = fmap nvec ps
    ts = filter (\l -> length l == 2) (subsequences vs)
    antipodals =
        filter (\t -> (realToFrac (vnorm (vadd (head t) (last t)) :: Double) :: Nano) == 0) ts
    nv = vunit $ foldl vadd vzero vs

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [Vector3d] -> [(Vector3d, Vector3d)]
egdes ps = zip ps (tail ps ++ [head ps])

lrph :: Length -> Length -> Double -> Length
lrph h0 h1 f = metres h
  where
    h0' = toMetres h0
    h1' = toMetres h1
    h = h0' + (h1' - h0') * f

intersections' :: (Spherical a) => Vector3d -> Vector3d -> a -> Maybe (Position a, Position a)
intersections' n1 n2 s
    | (vnorm i :: Double) == 0.0 = Nothing
    | otherwise
    , let ni = nvh (vunit i) zero s = Just (ni, antipode ni)
  where
    i = vcross n1 n2

-- | reference sphere radius.
radius :: (Spherical a) => Position a -> Length
radius = modelRadius . model

normal' :: (Spherical a) => Position a -> Position a -> Vector3d
normal' p1 p2 = vcross (nvec p1) (nvec p2)

signedAngle :: Vector3d -> Vector3d -> Maybe Vector3d -> Angle
signedAngle v1 v2 n = radians (signedAngleRadians v1 v2 n)
