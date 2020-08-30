-- |
-- Module:      Data.Geo.Jord.GreatCircle
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geographical Position calculations on great circles, i.e. using a __sphere__ to represent
-- the celestial body that positions refer to.
--
-- In order to use this module you should start with the following imports:
--
-- @
--     import Data.Geo.Jord.Geodetic
--     import Data.Geo.Jord.GreatCircle
-- @
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Point_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
--
-- FIXME: add projection, isLeft, turn, discretiseCircle & discretiseArc
module Data.Geo.Jord.GreatCircle
    (
    -- * The 'GreatCircle' type
      GreatCircle
    , through
    , headingOn
    -- * The 'MinorArc' type
    , MinorArc
    , minorArc
    , minorArcStart
    , minorArcEnd
    -- * Calculations
    , alongTrackDistance
    , alongTrackDistance'
    , angularDistance
    , crossTrackDistance
    , crossTrackDistance'
    , destination
    , finalBearing
    , initialBearing
    , interpolate
    , intersection
    , intersections
    , isInsideSurface
    , mean
    , surfaceDistance
    ) where

import Data.Fixed (Nano)
import Data.List (subsequences)

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Ellipsoid (equatorialRadius)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Math3d (V3)
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Spherical, surface)

-- | A circle on the __surface__ of a __sphere__ which lies in a plane
-- passing through the sphere centre. Every two distinct and non-antipodal points
-- define a unique Great Circle.
--
-- It is internally represented as its normal vector - i.e. the normal vector
-- to the plane containing the great circle.
--
data GreatCircle a =
    GreatCircle !V3 !a String
    deriving (Eq)

instance (Spherical a) => Show (GreatCircle a) where
    show (GreatCircle _ _ s) = s

-- | @greatCircleThrough p1 p2@ returns the 'GreatCircle' passing by both positions @p1@ and @p2@.
-- If positions are antipodal, any great circle passing through those positions will be returned.
-- Returns 'Nothing' if given positions are equal.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Geo.Jord.GreatCircle
-- >>> import qualified Data.Geo.Jord.Position
-- >>>
-- >>> let p1 = latLongHeightPos 45.0 (-143.5) (metres 1500) S84
-- >>> let p2 = latLongHeightPos 46.0 14.5 (metres 3000) S84
-- >>> GreatCircle.through p1 p2 -- heights are ignored, great circle is always at surface.
-- Just Great Circle { through 45°0'0.000"N,143°30'0.000"W 1500.0m (S84) & 46°0'0.000"N,14°30'0.000"E 3000.0m (S84) }
--
through ::
       (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Maybe (GreatCircle a)
through p1 p2 = fmap (\n -> GreatCircle n (Geodetic.model p1) dscr) (arcNormal p1 p2)
  where
    dscr = "Great Circle { through " ++ show p1 ++ " & " ++ show p2 ++ " }"

-- | @greatCircleHeadingOn p b@ returns the 'GreatCircle' passing by position @p@ and
-- heading on bearing @b@.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p = latLongPos 45.0 (-143.5) S84
-- >>> let b = decimalDegrees 33.0
-- >>> greatCircleHeadingOn p b
-- Great Circle { by 45°0'0.000"N,143°30'0.000"W 0.0m (S84) & heading on 33°0'0.000" }
--
headingOn :: (Spherical a) => Geodetic.Position a -> Angle -> GreatCircle a
headingOn p b = GreatCircle (Math3d.subtract n' e') m dscr
  where
    m = Geodetic.model p
    v = Geodetic.nvector p
    e = Math3d.cross (Geodetic.nvector . Geodetic.northPole $ m) v -- easting
    n = Math3d.cross v e -- northing
    e' = Math3d.scale e (Angle.cos b / Math3d.norm e)
    n' = Math3d.scale n (Angle.sin b / Math3d.norm n)
    dscr = "Great Circle { by " ++ show p ++ " & heading on " ++ show b ++ " }"

-- | Oriented minor arc of a great circle between two positions: shortest path between
-- positions on a great circle.
data MinorArc a =
    MinorArc !V3 (Geodetic.Position a) (Geodetic.Position a)
    deriving (Eq)

instance (Spherical a) => Show (MinorArc a) where
    show (MinorArc _ s e) = "Minor Arc { from: " ++ show s ++ ", to: " ++ show e ++ " }"

-- | @minorArc p1 p2@ returns the 'MinorArc' from @p1@ to @p2@.
-- Returns 'Nothing' if given positions are equal.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p1 = latLongHeightPos 45.0 (-143.5) (metres 1500) S84
-- >>> let p2 = latLongHeightPos 46.0 14.5 (metres 3000) S84
-- Just Minor Arc { from: 45°0'0.000"N,143°30'0.000"W 1500.0m (S84), to: 46°0'0.000"N,14°30'0.000"E 3000.0m (S84) }
--
minorArc :: (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Maybe (MinorArc a)
minorArc p1 p2 = fmap (\n -> MinorArc n p1 p2) (arcNormal p1 p2)

-- | @minorArcStart ma@ returns the start position of minor arc @ma@.
minorArcStart :: (Spherical a) => MinorArc a -> Geodetic.Position a
minorArcStart (MinorArc _ s _) = s

-- | @minorArcEnd ma@ returns the end position of minor arc @ma@.
minorArcEnd :: (Spherical a) => MinorArc a -> Geodetic.Position a
minorArcEnd (MinorArc _ _ e) = e

-- | @alongTrackDistance p a@ computes how far Position @p@ is along a path described
-- by the minor arc @a@: if a perpendicular is drawn from @p@  to the path, the
-- along-track distance is the signed distance from the start point to where the
-- perpendicular crosses the path.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p = s84Pos 53.2611 (-0.7972) zero
-- >>> let ma = minorArcBetween (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
-- >>> fmap (alongTrackDistance p) ma
-- Right 62.3315757km
--
alongTrackDistance :: (Spherical a) => Geodetic.Position a -> MinorArc a -> Length
alongTrackDistance p (MinorArc n s _) = alongTrackDistance'' p s n

-- | @alongTrackDistance' p s b@ computes how far Position @p@ is along a path starting
-- at @s@ and heading on bearing @b@: if a perpendicular is drawn from @p@  to the path, the
-- along-track distance is the signed distance from the start point to where the
-- perpendicular crosses the path.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p = s84Pos 53.2611 (-0.7972) zero
-- >>> let s = s84Pos 53.3206 (-1.7297) zero
-- >>> let b = decimalDegrees 96.0017325
-- >>> alongTrackDistance' p s b
-- 62.3315757km
--
alongTrackDistance' ::
       (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Angle -> Length
alongTrackDistance' p s b = alongTrackDistance'' p s n
  where
    (GreatCircle n _ _) = headingOn s b

-- | @angularDistance p1 p2 n@ computes the angle between the horizontal Points @p1@ and @p2@.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @p1@ is clockwise looking along @n@, - in opposite direction.
angularDistance ::
       (Spherical a)
    => Geodetic.Position a
    -> Geodetic.Position a
    -> Maybe (Geodetic.Position a)
    -> Angle
angularDistance p1 p2 n = Angle.betweenSigned v1 v2 vn
  where
    v1 = Geodetic.nvector p1
    v2 = Geodetic.nvector p2
    vn = fmap Geodetic.nvector n

-- | @crossTrackDistance p gc@ computes the signed distance from horizontal Position @p@ to great circle @gc@.
-- Returns a negative 'Length' if Position is left of great circle,
-- positive 'Length' if Position is right of great circle; the orientation of the
-- great circle is therefore important.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let gc1 = greatCircleThrough (s84Pos 51 0 zero) (s84Pos 52 1 zero)
-- >>> fmap (crossTrackDistance p) gc1
-- Right -176.7568725km
-- >>>
-- >>> let gc2 = greatCircleThrough (s84Pos 52 1 zero) (s84Pos 51 0 zero)
-- >>> fmap (crossTrackDistance p) gc2
-- Right 176.7568725km
-- >>>
-- >>> let p = s84Pos 53.2611 (-0.7972) zero
-- >>> let gc = greatCircleHeadingOn (s84Pos 53.3206 (-1.7297) zero) (decimalDegrees 96.0)
-- >>> crossTrackDistance p gc
-- -305.6629 metres
--
crossTrackDistance :: (Spherical a) => Geodetic.Position a -> GreatCircle a -> Length
crossTrackDistance p (GreatCircle n _ _) =
    Angle.arcLength (Angle.subtract a (Angle.decimalDegrees 90)) (radius p)
  where
    a = Angle.between n (Geodetic.nvector p)

-- | @crossTrackDistance' p s b@ computes the signed distance from horizontal Position @p@ to the
-- great circle passing by @s@ and heading on bearing @b@.
--
-- This is equivalent to:
--
-- @
--     'crossTrackDistance' p ('greatCircleHeadingOn' s b)
-- @
--
crossTrackDistance' ::
       (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Angle -> Length
crossTrackDistance' p s b = crossTrackDistance p (headingOn s b)

-- | @destination p b d@ computes the position along the great circle, reached from
-- position @p@ having travelled the __surface__ distance @d@ on the initial bearing (compass angle) @b@
-- at __constant__ height.
-- Note that the  bearing will normally vary before destination is reached.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> destination (s84Pos 54 154 (metres 15000)) (decimalDegrees 33) (kilometres 1000)
-- 61°10'44.188"N,164°10'19.254"E 15.0km (S84)
--
destination :: (Spherical a) => Geodetic.Position a -> Angle -> Length -> Geodetic.Position a
destination p b d
    | d == Length.zero = p
    | otherwise = Geodetic.nvectorHeightPos' nvd (Geodetic.height p) m
  where
    m = Geodetic.model p
    nv = Geodetic.nvector p
    ed = Math3d.unit (Math3d.cross (Geodetic.nvector . Geodetic.northPole $ m) nv) -- east direction vector at v
    nd = Math3d.cross nv ed -- north direction vector at v
    r = radius p
    ta = Angle.central d r -- central angle
    de = Math3d.add (Math3d.scale nd (Angle.cos b)) (Math3d.scale ed (Angle.sin b)) -- unit vector in the direction of the azimuth
    nvd = Math3d.add (Math3d.scale nv (Angle.cos ta)) (Math3d.scale de (Angle.sin ta))

-- | @surfaceDistance p1 p2@ computes the surface distance on the great circle between the
-- positions @p1@ and @p2@.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> surfaceDistance (northPole S84) (southPole S84)
-- 20015.114352233km
-- >>>
-- >>> surfaceDistance (northPole S84) (northPole S84)
-- 0.0m
--
surfaceDistance :: (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Length
surfaceDistance p1 p2 = Angle.arcLength a (radius p1)
  where
    a = Angle.between (Geodetic.nvector p1) (Geodetic.nvector p2)

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
-- The final bearing will differ from the initial bearing by varying degrees according to distance and latitude.
-- Returns 'Nothing' if both positions are equals.
--
-- ==== __Examples__
--
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p1 = s84Pos 0 1 (metres 12000)
-- >>> let p2 = s84Pos 0 0 (metres 5000)
-- >>> finalBearing p1 p2
-- Just 270°0'0.000"
-- >>>
-- >>> finalBearing p1 p1
-- Nothing
--
finalBearing :: (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Maybe Angle
finalBearing p1 p2
    | Geodetic.llEq p1 p2 = Nothing
    | otherwise = Just (Angle.normalise (initialBearing' p2 p1) (Angle.decimalDegrees 180))

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
-- Returns 'Nothing' if both positions are equals.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p1 = s84Pos 58.643889 (-5.714722) (metres 12000)
-- >>> let p2 = s84Pos 50.066389 (-5.714722) (metres 12000)
-- >>> initialBearing p1 p2
-- Just 180°0'0.000"
-- >>>
-- >>> initialBearing p1 p1
-- Nothing
--
initialBearing :: (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Maybe Angle
initialBearing p1 p2
    | Geodetic.llEq p1 p2 = Nothing
    | otherwise = Just (initialBearing' p1 p2)

-- | @interpolate p0 p1 f# computes the position at fraction @f@ between the @p0@ and @p1@.
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
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p1 = s84Pos 53.479444 (-2.245278) (metres 10000)
-- >>> let p2 = s84Pos 55.605833 13.035833 (metres 20000)
-- >>> interpolate p1 p2 0.5
-- 54°47'0.805"N,5°11'41.947"E 15.0km (S84)
--
interpolate ::
       (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Double -> Geodetic.Position a
interpolate p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = Geodetic.nvectorHeightPos' iv ih (Geodetic.model p0)
  where
    nv0 = Geodetic.nvector p0
    h0 = Geodetic.height p0
    nv1 = Geodetic.nvector p1
    h1 = Geodetic.height p1
    iv = Math3d.unit (Math3d.add nv0 (Math3d.scale (Math3d.subtract nv1 nv0) f))
    ih = lrph h0 h1 f

-- | Computes the intersection between the two given minor arcs of great circle.
--
-- see also 'intersections'
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let a1 = minorArcBetween (s84Pos 51.885 0.235 zero) (s84Pos 48.269 13.093 zero)
-- >>> let a2 = minorArcBetween (s84Pos 49.008 2.549 zero) (s84Pos 56.283 11.304 zero)
-- >>> join (intersection <$> a1 <*> a2)
-- Just 50°54'6.260"N,4°29'39.052"E 0.0m (S84)
--
intersection :: (Spherical a) => MinorArc a -> MinorArc a -> Maybe (Geodetic.Position a)
intersection a1@(MinorArc n1 s1 e1) a2@(MinorArc n2 s2 e2) =
    case intersections' n1 n2 (Geodetic.model s1) of
        Nothing -> Nothing
        (Just (i1, i2))
            | onMinorArc pot a1 && onMinorArc pot a2 -> Just pot
            | otherwise -> Nothing
            where mid =
                      Math3d.unit $
                      foldl
                          Math3d.add
                          Math3d.zero
                          [ Geodetic.nvector s1
                          , Geodetic.nvector e1
                          , Geodetic.nvector s2
                          , Geodetic.nvector e2
                          ]
                  pot =
                      if Math3d.dot (Geodetic.nvector i1) mid > 0
                          then i1
                          else i2

-- | Computes the intersections between the two given 'GreatCircle's.
-- Two great circles intersect exactly twice unless there are equal (regardless of orientation),
-- in which case 'Nothing' is returned.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let gc1 = greatCircleHeadingOn (s84Pos 51.885 0.235 zero) (decimalDegrees 108.63)
-- >>> let gc2 = greatCircleHeadingOn (s84Pos 49.008 2.549 zero) (decimalDegrees 32.72)
-- >>> intersections gc1 gc2
-- Just (50°54'6.201"N,4°29'39.402"E 0.0m (S84),50°54'6.201"S,175°30'20.598"W 0.0m (S84))
-- >>> let i = intersections gc1 gc2
-- fmap fst i == fmap (antipode . snd) i
-- >>> True
--
intersections ::
       (Spherical a)
    => GreatCircle a
    -> GreatCircle a
    -> Maybe (Geodetic.Position a, Geodetic.Position a)
intersections (GreatCircle n1 m _) (GreatCircle n2 _ _) = intersections' n1 n2 m

-- | @isInsideSurface p ps@ determines whether position @p@ is inside the __surface__ polygon defined by
-- positions @ps@ (i.e. ignoring the height of the positions).
-- The polygon can be opened or closed (i.e. if @head ps /= last ps@).
--
-- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
-- will sum to less than 360°, and exterior point angles will be small but non-zero.
--
-- Always returns 'False' if @ps@ does not at least defines a triangle or if @p@ is any of the @ps@.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.GreatCircle
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let malmo = s84Pos 55.6050 13.0038 zero
-- >>> let ystad = s84Pos 55.4295 13.82 zero
-- >>> let lund = s84Pos 55.7047 13.1910 zero
-- >>> let helsingborg = s84Pos 56.0465 12.6945 zero
-- >>> let kristianstad = s84Pos 56.0294 14.1567 zero
-- >>> let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
-- >>> let hoor = s84Pos 55.9295 13.5297 zero
-- >>> let hassleholm = s84Pos 56.1589 13.7668 zero
-- >>> isInsideSurface hoor polygon
-- True
-- >>> isInsideSurface hassleholm polygon
-- False
--
isInsideSurface :: (Spherical a) => Geodetic.Position a -> [Geodetic.Position a] -> Bool
isInsideSurface p ps
    | null ps = False
    | any (Geodetic.llEq p) ps = False
    | Geodetic.llEq (head ps) (last ps) = isInsideSurface p (init ps)
    | length ps < 3 = False
    | otherwise =
        let aSum =
                foldl
                    (\a v' -> Angle.add a (uncurry Angle.betweenSigned v' (Just v)))
                    Angle.zero
                    (egdes (map (Math3d.subtract v) vs))
         in abs (Angle.toDecimalDegrees aSum) > 180.0
  where
    v = Geodetic.nvector p
    vs = fmap Geodetic.nvector ps

-- | @mean ps@ computes the geographic mean surface position of @ps@, if it is defined.
--
-- The geographic mean is not defined for antipodals positions (since they
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
mean :: (Spherical a) => [Geodetic.Position a] -> Maybe (Geodetic.Position a)
mean [] = Nothing
mean [p] = Just p
mean ps =
    if null antipodals
        then Just (Geodetic.nvectorPos' nv (Geodetic.model . head $ ps))
        else Nothing
  where
    vs = fmap Geodetic.nvector ps
    ts = filter (\l -> length l == 2) (subsequences vs)
    antipodals =
        filter
            (\t -> (realToFrac (Math3d.norm (Math3d.add (head t) (last t)) :: Double) :: Nano) == 0)
            ts
    nv = Math3d.unit $ foldl Math3d.add Math3d.zero vs

-- private
alongTrackDistance'' :: (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> V3 -> Length
alongTrackDistance'' p s n = Angle.arcLength a (radius s)
  where
    a =
        Angle.betweenSigned
            (Geodetic.nvector s)
            (Math3d.cross (Math3d.cross n (Geodetic.nvector p)) n)
            (Just n)

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [V3] -> [(V3, V3)]
egdes ps = zip ps (tail ps ++ [head ps])

lrph :: Length -> Length -> Double -> Length
lrph h0 h1 f = Length.metres h
  where
    h0' = Length.toMetres h0
    h1' = Length.toMetres h1
    h = h0' + (h1' - h0') * f

intersections' :: (Spherical a) => V3 -> V3 -> a -> Maybe (Geodetic.Position a, Geodetic.Position a)
intersections' n1 n2 s
    | (Math3d.norm i :: Double) == 0.0 = Nothing
    | otherwise
    , let ni = Geodetic.nvectorPos' (Math3d.unit i) s = Just (ni, Geodetic.antipode ni)
  where
    i = Math3d.cross n1 n2

initialBearing' :: (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Angle
initialBearing' p1 p2 = Angle.normalise a (Angle.decimalDegrees 360)
  where
    m = Geodetic.model p1
    v1 = Geodetic.nvector p1
    v2 = Geodetic.nvector p2
    gc1 = Math3d.cross v1 v2 -- great circle through p1 & p2
    gc2 = Math3d.cross v1 (Geodetic.nvector . Geodetic.northPole $ m) -- great circle through p1 & north pole
    a = Angle.betweenSigned gc1 gc2 (Just v1)

arcNormal :: (Spherical a) => Geodetic.Position a -> Geodetic.Position a -> Maybe V3
arcNormal p1 p2
    | Geodetic.llEq p1 p2 = Nothing
    | Geodetic.llEq (Geodetic.antipode p1) p2 = Nothing
    | otherwise = Just (Math3d.cross (Geodetic.nvector p1) (Geodetic.nvector p2))

-- | @onMinorArc p a@ determines whether position @p@ is on the minor arc @a@.
-- Warning: this function assumes that @p@ is on great circle of the minor arc.
-- return true if chord lengths between (p, start) & (p, end) are both less than
-- chord length between (start, end)
onMinorArc :: (Spherical a) => Geodetic.Position a -> MinorArc a -> Bool
onMinorArc p (MinorArc _ s e) =
    Math3d.squaredDistance v0 v1 <= l && Math3d.squaredDistance v0 v2 <= l
  where
    v0 = Geodetic.nvector p
    v1 = Geodetic.nvector s
    v2 = Geodetic.nvector e
    l = Math3d.squaredDistance v1 v2

-- | reference sphere radius.
radius :: (Spherical a) => Geodetic.Position a -> Length
radius = equatorialRadius . surface . Geodetic.model
