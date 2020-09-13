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
-- import qualified Data.Geo.Jord.Geodetic as Geodetic
-- import qualified Data.Geo.Jord.GreatCircle as GreatCircle
-- @
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Point_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
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
    , Side(..)
    , alongTrackDistance
    , alongTrackDistance'
    , angularDistance
    , crossTrackDistance
    , crossTrackDistance'
    , destination
    , distance
    , enclosedBy
    , finalBearing
    , initialBearing
    , interpolated
    , intersection
    , intersections
    , mean
    , projection
    , side
    , turn
    ) where

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Ellipsoid (equatorialRadius)
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Math3d (V3)
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Spherical, surface)

-- | A circle on the surface of a __sphere__ which lies in a plane
-- passing through the sphere centre. Every two distinct and non-antipodal points
-- define a unique Great Circle.
--
-- It is internally represented as its normal vector - i.e. the normal vector
-- to the plane containing the great circle.
data GreatCircle a =
    GreatCircle !V3 !a String
    deriving (Eq)

instance (Spherical a) => Show (GreatCircle a) where
    show (GreatCircle _ _ s) = s

-- | @through p1 p2@ returns the 'GreatCircle' passing by both positions @p1@ and @p2@ (in this direction). For example:
--
-- >>> let p1 = Geodetic.s84Pos 45.0 (-143.5)
-- >>> let p2 = Geodetic.s84Pos 46.0 14.5
-- >>> GreatCircle.through p1 p2
-- Just Great Circle { through 45°0'0.000"N,143°30'0.000"W (S84) & 46°0'0.000"N,14°30'0.000"E (S84) }
--
-- Returns 'Nothing' if given positions are equal or @p1@ is antipode of @p2@.
through :: (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Maybe (GreatCircle a)
through p1 p2 = fmap (\n -> GreatCircle n (Geodetic.model p1) dscr) (arcNormal p1 p2)
  where
    dscr = "Great Circle { through " ++ show p1 ++ " & " ++ show p2 ++ " }"

-- | @headingOn p b@ returns the 'GreatCircle' passing by position @p@ and heading on bearing @b@. For example:
--
-- >>> let p = Geodetic.s84Pos 45.0 (-143.5)
-- >>> let b = Angle.decimalDegrees 33.0
-- >>> GreatCircle.headingOn p b
-- Great Circle { by 45°0'0.000"N,143°30'0.000"W (S84) & heading on 33°0'0.000" }
headingOn :: (Spherical a) => HorizontalPosition a -> Angle -> GreatCircle a
headingOn p b = GreatCircle (Math3d.subtract n' e') m dscr
  where
    m = Geodetic.model p
    v = Geodetic.nvector p
    e = Math3d.cross (Geodetic.nvector . Geodetic.northPole $ m) v -- easting
    n = Math3d.cross v e -- northing
    e' = Math3d.scale e (Angle.cos b / Math3d.norm e)
    n' = Math3d.scale n (Angle.sin b / Math3d.norm n)
    dscr = "Great Circle { by " ++ show p ++ " & heading on " ++ show b ++ " }"

-- | Oriented minor arc of a great circle between two positions: shortest path between positions on a great circle.
data MinorArc a =
    MinorArc !V3 (HorizontalPosition a) (HorizontalPosition a)
    deriving (Eq)

instance (Spherical a) => Show (MinorArc a) where
    show (MinorArc _ s e) = "Minor Arc { from: " ++ show s ++ ", to: " ++ show e ++ " }"

-- | @minorArc p1 p2@ returns the 'MinorArc' from @p1@ to @p2@.  For example:
--
-- >>> let p1 = Geodetic.s84Pos 45.0 (-143.5)
-- >>> let p2 = Geodetic.s84Pos 46.0 14.5
-- >>> GreatCircle.minorArc p1 p2
-- Just Minor Arc { from: 45°0'0.000"N,143°30'0.000"W (S84), to: 46°0'0.000"N,14°30'0.000"E (S84) }
--
-- Returns 'Nothing' if given positions are equal.
minorArc :: (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Maybe (MinorArc a)
minorArc p1 p2 = fmap (\n -> MinorArc n p1 p2) (arcNormal p1 p2)

-- | @minorArcStart ma@ returns the start position of minor arc @ma@.
minorArcStart :: (Spherical a) => MinorArc a -> HorizontalPosition a
minorArcStart (MinorArc _ s _) = s

-- | @minorArcEnd ma@ returns the end position of minor arc @ma@.
minorArcEnd :: (Spherical a) => MinorArc a -> HorizontalPosition a
minorArcEnd (MinorArc _ _ e) = e

-- | Side of a position w.r.t. to a great circle.
data Side
    = LeftOf -- ^ position is left of the great circle.
    | RightOf -- ^ position is right of the great circle.
    | None -- ^ position is on the great circle, or the great circle is undefined.
    deriving (Eq, Show)

-- | @alongTrackDistance p a@ computes how far position @p@ is along a path described by the minor arc @a@: if a
-- perpendicular is drawn from @p@  to the path, the along-track distance is the signed distance from the start point to
-- where the perpendicular crosses the path. For example:
--
-- >>> let p = Geodetic.s84Pos 53.2611 (-0.7972)
-- >>> let mas = Geodetic.s84Pos 53.3206 (-1.7297)
-- >>> let mae = Geodetic.s84Pos 53.1887 0.1334
-- >>> fmap (GreatCircle.alongTrackDistance p) (GreatCircle.minorArc mas mae)
-- Just 62.3315791km
alongTrackDistance :: (Spherical a) => HorizontalPosition a -> MinorArc a -> Length
alongTrackDistance p (MinorArc n s _) = alongTrackDistance'' p s n

-- | @alongTrackDistance' p s b@ computes how far Position @p@ is along a path starting at @s@ and heading on
-- bearing @b@: if a perpendicular is drawn from @p@  to the path, the along-track distance is the signed distance from
-- the start point to where the perpendicular crosses the path. For example:
--
-- >>> let p = Geodetic.s84Pos 53.2611 (-0.7972)
-- >>> let s = Geodetic.s84Pos 53.3206 (-1.7297)
-- >>> let b = Angle.decimalDegrees 96.0017325
-- >>> GreatCircle.alongTrackDistance' p s b
-- 62.3315791km
alongTrackDistance' ::
       (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Angle -> Length
alongTrackDistance' p s b = alongTrackDistance'' p s n
  where
    (GreatCircle n _ _) = headingOn s b

-- | @angularDistance p1 p2 n@ computes the angle between the horizontal Points @p1@ and @p2@.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180], signed + if @p1@ is clockwis
-- looking along @n@, - in opposite direction.
angularDistance ::
       (Spherical a)
    => HorizontalPosition a
    -> HorizontalPosition a
    -> Maybe (HorizontalPosition a)
    -> Angle
angularDistance p1 p2 n = signedAngleBetween v1 v2 vn
  where
    v1 = Geodetic.nvector p1
    v2 = Geodetic.nvector p2
    vn = fmap Geodetic.nvector n

-- | @crossTrackDistance p gc@ computes the signed distance from horizontal Position @p@ to great circle @gc@.
-- Returns a negative 'Length' if Position is left of great circle, positive 'Length' if Position is right
-- of great circle; the orientation of the great circle is therefore important. For example:
--
-- >>> let p = Geodetic.s84Pos 53.2611 (-0.7972)
-- >>> let gc1 = GreatCircle.through (Geodetic.s84Pos 51 0) (Geodetic.s84Pos 52 1)
-- >>> fmap (GreatCircle.crossTrackDistance p) gc1
-- Just -176.756870526km
-- >>> let gc2 = GreatCircle.through (Geodetic.s84Pos 52 1) (Geodetic.s84Pos 51 0)
-- >>> fmap (GreatCircle.crossTrackDistance p) gc2
-- Just 176.7568725km
-- >>> let gc3 = GreatCircle.headingOn (Geodetic.s84Pos 53.3206 (-1.7297)) (Angle.decimalDegrees 96.0)
-- >>> GreatCircle.crossTrackDistance p gc3
-- -305.665267m metres
crossTrackDistance :: (Spherical a) => HorizontalPosition a -> GreatCircle a -> Length
crossTrackDistance p (GreatCircle n _ _) =
    Angle.arcLength (Angle.subtract a (Angle.decimalDegrees 90)) (radius p)
  where
    a = angleBetween n (Geodetic.nvector p)

-- | @crossTrackDistance' p s b@ computes the signed distance from horizontal Position @p@ to the great circle passing
-- by @s@ and heading on bearing @b@.
--
-- This is equivalent to:
--
-- > GreatCircle.crossTrackDistance p (GreatCircle.headingOn s b)
crossTrackDistance' ::
       (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Angle -> Length
crossTrackDistance' p s b = crossTrackDistance p (headingOn s b)

-- | @destination p b d@ computes the position along the great circle, reached from position @p@ having travelled the
-- distance @d@ on the initial bearing (compass angle) @b@. For example:
--
-- >>> let p = Geodetic.s84Pos 54 154
-- >>> GreatCircle.destination p (Angle.decimalDegrees 33) (Length.kilometres 1000)
-- 61°10'44.188"N,164°10'19.254"E (S84)
--
-- Note that the bearing will normally vary before destination is reached.
destination :: (Spherical a) => HorizontalPosition a -> Angle -> Length -> HorizontalPosition a
destination p b d
    | d == Length.zero = p
    | otherwise = Geodetic.nvectorPos' nvd m
  where
    m = Geodetic.model p
    nv = Geodetic.nvector p
    ed = Math3d.unit (Math3d.cross (Geodetic.nvector . Geodetic.northPole $ m) nv) -- east direction vector at v
    nd = Math3d.cross nv ed -- north direction vector at v
    r = radius p
    ta = Angle.central d r -- central angle
    de = Math3d.add (Math3d.scale nd (Angle.cos b)) (Math3d.scale ed (Angle.sin b)) -- unit vector in the direction of the azimuth
    nvd = Math3d.add (Math3d.scale nv (Angle.cos ta)) (Math3d.scale de (Angle.sin ta))

-- | @distance p1 p2@ computes the surface distance on the great circle between the positions @p1@ and @p2@. For example:
--
-- >>> GreatCircle.distance (Geodetic.northPole S84) (Geodetic.southPole S84)
-- 20015.114352233km
-- >>> GreatCircle.distance (Geodetic.northPole S84) (Geodetic.northPole S84)
-- 0.0m
distance :: (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Length
distance p1 p2 = Angle.arcLength a (radius p1)
  where
    a = angleBetween (Geodetic.nvector p1) (Geodetic.nvector p2)

-- | @enclosedBy p ps@ determines whether position @p@ is enclosed by the polygon defined by horizontal positions @ps@.
-- The polygon can be opened or closed (i.e. if @head ps /= last ps@).
--
-- Uses the angle summation test: on a sphere, due to spherical excess, enclosed point angles
-- will sum to less than 360°, and exterior point angles will be small but non-zero.
--
-- Always returns 'False' if @ps@ does not at least defines a triangle or if @p@ is any of the @ps@.
--
-- ==== __Examples__
--
-- >>> let malmo = Geodetic.s84Pos 55.6050 13.0038
-- >>> let ystad = Geodetic.s84Pos 55.4295 13.82
-- >>> let lund = Geodetic.s84Pos 55.7047 13.1910
-- >>> let helsingborg = Geodetic.s84Pos 56.0465 12.6945
-- >>> let kristianstad = Geodetic.s84Pos 56.0294 14.1567
-- >>> let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
-- >>> let hoor = Geodetic.s84Pos 55.9295 13.5297
-- >>> let hassleholm = Geodetic.s84Pos 56.1589 13.7668
-- >>> GreatCircle.enclosedBy hoor polygon
-- True
-- >>> GreatCircle.enclosedBy hassleholm polygon
-- False
enclosedBy :: (Spherical a) => HorizontalPosition a -> [HorizontalPosition a] -> Bool
enclosedBy p ps
    | null ps = False
    | p `elem` ps = False
    | head ps == last ps = enclosedBy p (init ps)
    | length ps < 3 = False
    | otherwise =
        let aSum =
                foldl
                    (\a v' -> Angle.add a (uncurry signedAngleBetween v' (Just v)))
                    Angle.zero
                    (egdes (map (Math3d.subtract v) vs))
         in abs (Angle.toDecimalDegrees aSum) > 180.0
  where
    v = Geodetic.nvector p
    vs = fmap Geodetic.nvector ps

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
-- The final bearing will differ from the initial bearing by varying degrees according to distance and latitude.
-- For example:
--
-- >>> let p1 = Geodetic.s84Pos 0 1
-- >>> let p2 = Geodetic.s84Pos 0 0
-- >>> GreatCircle.finalBearing p1 p2
-- Just 270°0'0.000"
--
-- Returns 'Nothing' if both positions are equals.
finalBearing :: (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Maybe Angle
finalBearing p1 p2
    | p1 == p2 = Nothing
    | otherwise = Just (Angle.normalise (initialBearing' p2 p1) (Angle.decimalDegrees 180))

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
-- For example:
--
-- >>> let p1 = Geodetic.s84Pos 58.643889 (-5.714722)
-- >>> let p2 = Geodetic.s84Pos 50.066389 (-5.714722)
-- >>> GreatCircle.initialBearing p1 p2
-- Just 180°0'0.000"
--
-- Returns 'Nothing' if both positions are equals.
initialBearing :: (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Maybe Angle
initialBearing p1 p2
    | p1 == p2 = Nothing
    | otherwise = Just (initialBearing' p1 p2)

-- | @interpolated p0 p1 f# computes the position at fraction @f@ between the @p0@ and @p1@. For example:
--
-- >>> let p1 = Geodetic.s84Pos 53.479444 (-2.245278)
-- >>> let p2 = Geodetic.s84Pos 55.605833 13.035833
-- >>> GreatCircle.interpolated p1 p2 0.5
-- 54°47'0.805"N,5°11'41.947"E (S84)
--
-- Special conditions:
--
-- @
-- interpolated p0 p1 0.0 = p0
-- interpolated p0 p1 1.0 = p1
-- 'error's if @f < 0 || f > 1@
-- @
interpolated ::
       (Spherical a)
    => HorizontalPosition a
    -> HorizontalPosition a
    -> Double
    -> HorizontalPosition a
interpolated p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = Geodetic.nvectorPos' iv (Geodetic.model p0)
  where
    nv0 = Geodetic.nvector p0
    nv1 = Geodetic.nvector p1
    iv = Math3d.unit (Math3d.add nv0 (Math3d.scale (Math3d.subtract nv1 nv0) f))

-- | Computes the intersection between the two given minor arcs of great circle. For example:
--
-- >>> let a1s = Geodetic.s84Pos 51.885 0.235
-- >>> let a1e = Geodetic.s84Pos 48.269 13.093
-- >>> let a2s = Geodetic.s84Pos 49.008 2.549
-- >>> let a2e = Geodetic.s84Pos 56.283 11.304
-- >>> GreatCircle.intersection <$> (GreatCircle.minorArc a1s a1e) <*> (GreatCircle.minorArc a2s a2e)
-- Just (Just 50°54'6.260"N,4°29'39.052"E (S84))
--
-- see also 'intersections'
intersection :: (Spherical a) => MinorArc a -> MinorArc a -> Maybe (HorizontalPosition a)
intersection a1@(MinorArc n1 s1 e1) a2@(MinorArc n2 s2 e2) =
    case intersections' n1 n2 (Geodetic.model s1) of
        Nothing -> Nothing
        (Just (i1, i2))
            | onMinorArc pot a1 && onMinorArc pot a2 -> Just pot
            | otherwise -> Nothing
            where mid =
                      meanV
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
-- Two great circles intersect exactly twice unless there are equal (regardless of orientation), in which case 'Nothing'
-- is returned. For example:
--
-- >>> let gc1 = GreatCircle.headingOn (Geodetic.s84Pos 51.885 0.235) (Angle.decimalDegrees 108.63)
-- >>> let gc2 = GreatCircle.headingOn (Geodetic.s84Pos 49.008 2.549) (Angle.decimalDegrees 32.72)
-- >>> GreatCircle.intersections gc1 gc2
-- Just (50°54'6.201"N,4°29'39.401"E (S84),50°54'6.201"S,175°30'20.598"W (S84))
-- >>> let is = GreatCircle.intersections gc1 gc2
-- >>> fmap fst is == fmap (Geodetic.antipode . snd) is
-- True
intersections ::
       (Spherical a)
    => GreatCircle a
    -> GreatCircle a
    -> Maybe (HorizontalPosition a, HorizontalPosition a)
intersections (GreatCircle n1 m _) (GreatCircle n2 _ _) = intersections' n1 n2 m

-- | @mean ps@ computes the geographic mean horizontal position of @ps@, if it is defined. For example:
--
-- >>> let ps =
--             [ Geodetic.s84Pos 90 0
--             , Geodetic.s84Pos 60 10
--             , Geodetic.s84Pos 50 (-20)
--             ]
-- >>> GreatCircle.mean ps
-- Just 67°14'10.150"N,6°55'3.040"W (S84)
--
-- The geographic mean is not defined for antipodals positions (since they
-- cancel each other).
--
-- Special conditions:
--
-- @
-- mean [] = Nothing
-- mean [p] = Just p
-- mean [p1, .., antipode p1] = Nothing
-- @
mean :: (Spherical a) => [HorizontalPosition a] -> Maybe (HorizontalPosition a)
mean [] = Nothing
mean [p] = Just p
mean ps =
    if any (`elem` ps) antipodes
        then Nothing
        else Just
                 (Geodetic.nvectorPos'
                      (meanV (fmap Geodetic.nvector ps))
                      (Geodetic.model . head $ ps))
  where
    antipodes = fmap Geodetic.antipode ps

-- | @projection p ma@ computes the projection of the position @p@ on the minor arc @ma@. Returns 'Nothing' if the
-- position @p@ is the normal of the minor arc or if the projection is not within the minor arc @ma@ (including start
-- & end). For example:
--
-- >>> let p = Geodetic.s84Pos 53.2611 (-0.7972)
-- >>> let ma = fromJust (GreatCircle.minorArc (Geodetic.s84Pos 53.3206 (-1.7297)) (Geodetic.s84Pos 53.1887 0.1334))
-- >>> GreatCircle.projection p ma
-- Just Geodetic.s84Pos 53.25835330666666 (-0.7977433863888889)
projection :: (Spherical a) => HorizontalPosition a -> MinorArc a -> Maybe (HorizontalPosition a)
projection p ma@(MinorArc na mas mae) =
    case mnb of
        Nothing -> Nothing
        (Just nb) ->
            case is of
                Nothing -> Nothing
                (Just (i1, i2)) ->
                    if onMinorArc pot ma
                        then Just pot
                        else Nothing
                    where mid =
                              meanV
                                  [ Geodetic.nvector mas
                                  , Geodetic.nvector mae
                                  , Geodetic.nvector nap
                                  , Geodetic.nvector nbp
                                  ]
                          pot =
                              if Math3d.dot mid (Geodetic.nvector i1) > 0
                                  then i1
                                  else i2
            where nbp = Geodetic.nvectorPos' nb m -- ensure resolution of lat, lon
                  is = intersections' (Geodetic.nvector nap) (Geodetic.nvector nbp) m
  where
    m = Geodetic.model p
    nap = Geodetic.nvectorPos' na m -- ensure resolution of lat, lon
    mnb = arcNormal nap p -- normal to great circle (na, p) - if na is p or antipode of p, then projection is not possible.

-- | @side p0 p1 p2@ determines whether @p0@ is left of, right of or on the great circle passing through @p1@ and @p2@ (in this
-- direction). For example:
--
-- >>> GreatCircle.side (Geodetic.s84Pos 10 10) (Geodetic.s84Pos 0 0) (Geodetic.northPole S84)
-- RightOf
-- >>> GreatCircle.side (Geodetic.s84Pos 10 (-10)) (Geodetic.s84Pos 0 0) (Geodetic.northPole S84)
-- LeftOf
-- >>> GreatCircle.side (Geodetic.s84Pos 10 0) (Geodetic.s84Pos 0 0) (Geodetic.northPole S84)
-- None
-- Returns 'None' if @p1@ & @p2@ do not define a great circle (see 'through') or if any of the three position are equal.
side ::
       (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> HorizontalPosition a -> Side
side p0 p1 p2
    | p0 == p1 || p0 == p2 = None
    | otherwise = maybe None toSide ms
  where
    ms = fmap (Math3d.dot (Geodetic.nvector p0)) (arcNormal p1 p2)

-- | @turn a b c@ computes the angle turned from AB to BC; the angle is positive for left turn, negative for right turn
-- and 0 if all 3 positions are aligned or if any 2 positions are equal. For example:
--
-- >>> GreatCircle.turn (Geodetic.s84Pos 0 0) (Geodetic.s84Pos 45 0) (Geodetic.s84Pos 60 (-10))
-- 18°11'33.741"
turn ::
       (Spherical a)
    => HorizontalPosition a
    -> HorizontalPosition a
    -> HorizontalPosition a
    -> Angle
turn a b c =
    case ns of
        (Just n1, Just n2) -> signedAngleBetween n1 n2 (Just (Geodetic.nvector b))
        (_, _) -> Angle.zero
  where
    ns = (fmap Math3d.unit (arcNormal a b), fmap Math3d.unit (arcNormal b c))

-- private
alongTrackDistance'' ::
       (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> V3 -> Length
alongTrackDistance'' p s n = Angle.arcLength a (radius s)
  where
    a =
        signedAngleBetween
            (Geodetic.nvector s)
            (Math3d.cross (Math3d.cross n (Geodetic.nvector p)) n)
            (Just n)

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [V3] -> [(V3, V3)]
egdes ps = zip ps (tail ps ++ [head ps])

intersections' ::
       (Spherical a) => V3 -> V3 -> a -> Maybe (HorizontalPosition a, HorizontalPosition a)
intersections' n1 n2 s
    | (Math3d.norm i :: Double) == 0.0 = Nothing
    | otherwise
    , let ni = Geodetic.nvectorPos' (Math3d.unit i) s = Just (ni, Geodetic.antipode ni)
  where
    i = Math3d.cross n1 n2

initialBearing' :: (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Angle
initialBearing' p1 p2 = Angle.normalise a (Angle.decimalDegrees 360)
  where
    m = Geodetic.model p1
    v1 = Geodetic.nvector p1
    v2 = Geodetic.nvector p2
    gc1 = Math3d.cross v1 v2 -- great circle through p1 & p2
    gc2 = Math3d.cross v1 (Geodetic.nvector . Geodetic.northPole $ m) -- great circle through p1 & north pole
    a = signedAngleBetween gc1 gc2 (Just v1)

arcNormal :: (Spherical a) => HorizontalPosition a -> HorizontalPosition a -> Maybe V3
arcNormal p1 p2
    | p1 == p2 = Nothing
    | Geodetic.antipode p1 == p2 = Nothing
    | otherwise = Just (Math3d.cross (Geodetic.nvector p1) (Geodetic.nvector p2))

-- | @onMinorArc p a@ determines whether position @p@ is on the minor arc @a@.
-- Warning: this function assumes that @p@ is on great circle of the minor arc.
-- return true if chord lengths between (p, start) & (p, end) are both less than
-- chord length between (start, end)
onMinorArc :: (Spherical a) => HorizontalPosition a -> MinorArc a -> Bool
onMinorArc p (MinorArc _ s e) =
    Math3d.squaredDistance v0 v1 <= l && Math3d.squaredDistance v0 v2 <= l
  where
    v0 = Geodetic.nvector p
    v1 = Geodetic.nvector s
    v2 = Geodetic.nvector e
    l = Math3d.squaredDistance v1 v2

-- | reference sphere radius.
radius :: (Spherical a) => HorizontalPosition a -> Length
radius = equatorialRadius . surface . Geodetic.model

-- | angle between 2 vectors.
angleBetween :: V3 -> V3 -> Angle
angleBetween v1 v2 = signedAngleBetween v1 v2 Nothing

-- | Signed angle between 2 vectors.
-- If @n@ is 'Nothing', the angle is always in [0..pi], otherwise it is in [-pi, +pi],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
signedAngleBetween :: V3 -> V3 -> Maybe V3 -> Angle
signedAngleBetween v1 v2 n = Angle.atan2 sinO cosO
  where
    sign = maybe 1 (signum . Math3d.dot (Math3d.cross v1 v2)) n
    sinO = sign * Math3d.norm (Math3d.cross v1 v2)
    cosO = Math3d.dot v1 v2

meanV :: [Math3d.V3] -> V3
meanV vs = Math3d.unit $ foldl Math3d.add Math3d.zero vs

toSide :: Double -> Side
toSide s
    | s < 0 = RightOf
    | s > 0 = LeftOf
    | otherwise = None
