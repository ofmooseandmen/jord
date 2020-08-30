-- |
-- Module:      Data.Geo.Jord.Kinematics
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with kinematics calculations assuming a __spherical__ celestial body.
--
-- In order to use this module you should start with the following imports:
--
-- @
--     import Data.Geo.Jord.Geodetic
--     import Data.Geo.Jord.Kinematics
-- @
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
-- and in <https://calhoun.nps.edu/bitstream/handle/10945/29516/sometacticalalgo00shud.pdf Shudde, Rex H. (1986). Some tactical algorithms for spherical geometry>
--
-- FIXME: provide accessors for Course type
module Data.Geo.Jord.Kinematics
    (
    -- * The 'Track' type.
      Track(..)
    -- * The 'Course' type.
    , Course
    -- * The 'Cpa' type.
    , Cpa
    , cpaTime
    , cpaDistance
    , cpaPosition1
    , cpaPosition2
    -- * The 'Intercept' type.
    , Intercept
    , interceptTime
    , interceptDistance
    , interceptPosition
    , interceptorBearing
    , interceptorSpeed
    -- * Calculations
    , course
    , positionAfter
    , trackPositionAfter
    , cpa
    , intercept
    , interceptBySpeed
    , interceptByTime
    -- * re-exported for convenience
    , module Data.Geo.Jord.Duration
    , module Data.Geo.Jord.Speed
    ) where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust, isNothing)

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Duration (Duration)
import qualified Data.Geo.Jord.Duration as Duration (seconds, toMilliseconds, toSeconds, zero)
import Data.Geo.Jord.Ellipsoid (equatorialRadius)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle (initialBearing, surfaceDistance)
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (toMetres, zero)
import Data.Geo.Jord.Math3d (V3(..))
import qualified Data.Geo.Jord.Math3d as Math3d (add, dot, dotM, scale)
import Data.Geo.Jord.Model (Spherical, surface)
import Data.Geo.Jord.Speed (Speed)
import qualified Data.Geo.Jord.Speed as Speed (average, toMetresPerSecond)

-- | 'Track' represents the state of a vehicle by its current position, bearing and speed.
data Track a =
    Track
        { trackPosition :: Geodetic.Position a -- ^ position of the track.
        , trackBearing :: Angle -- ^ bearing of the track.
        , trackSpeed :: Speed -- ^ speed of the track.
        }
    deriving (Eq, Show)

-- | 'Course' represents the cardinal direction in which the vehicle is to be steered.
newtype Course =
    Course V3
    deriving (Eq, Show)

-- | Time to, and distance at, closest point of approach (CPA) as well as position of both tracks at CPA.
data Cpa a =
    Cpa
        { cpaTime :: Duration -- ^ time to CPA.
        , cpaDistance :: Length -- ^ distance at CPA.
        , cpaPosition1 :: Geodetic.Position a -- ^ position of track 1 at CPA.
        , cpaPosition2 :: Geodetic.Position a -- ^ position of track 2 at CPA.
        }
    deriving (Eq, Show)

-- | Time, distance and position of intercept as well as speed and initial bearing of interceptor.
data Intercept a =
    Intercept
        { interceptTime :: Duration -- ^ time to intercept.
        , interceptDistance :: Length -- ^ distance at intercept.
        , interceptPosition :: Geodetic.Position a -- ^ position of intercept.
        , interceptorBearing :: Angle -- ^ initial bearing of interceptor.
        , interceptorSpeed :: Speed -- ^ speed of interceptor.
        }
    deriving (Eq, Show)

-- | @course p b@ computes the course of a vehicle currently at position @p@ and following bearing @b@.
course :: (Spherical a) => Geodetic.Position a -> Angle -> Course
course p b = Course (V3 (vz (head r)) (vz (r !! 1)) (vz (r !! 2)))
  where
    lat = Geodetic.latitude p
    lon = Geodetic.longitude p
    r = Math3d.dotM (Math3d.dotM (rz (Angle.negate lon)) (ry lat)) (rx b)

-- | @positionAfter p b s d@ computes the position of a vehicle currently at position @p@
-- following bearing @b@ and travelling at speed @s@ after duration @d@ has elapsed assuming
-- the vehicle maintains a __constant__ altitude.
--
-- @positionAfter p b s d@ is a shortcut for @positionAfter' ('course' p b) s d@.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Kinematics
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p = s84Pos 53.321 (-1.729) (metres 15000)
-- >>> let b = decimalDegrees 96.0217
-- >>> let s = kilometresPerHour 124.8
-- >>> positionAfter p b s (hours 1)
-- 53°11'19.368"N,0°8'2.457"E 15.0km (S84)
-- @
positionAfter ::
       (Spherical a) => Geodetic.Position a -> Angle -> Speed -> Duration -> Geodetic.Position a
positionAfter p b s d = position' p (course p b) s (Duration.toSeconds d)

-- | @positionAfter p c s d@ computes the position of a vehicle currently at position @p@
-- on course @c@ and travelling at speed @s@ after duration @d@ has elapsed assuming
-- the vehicle maintains a __constant__ altitude.
positionAfter' ::
       (Spherical a) => Geodetic.Position a -> Course -> Speed -> Duration -> Geodetic.Position a
positionAfter' p c s d = position' p c s (Duration.toSeconds d)

-- | @trackPositionAfter t d@ computes the position of a track @t@ after duration @d@ has elapsed
-- assuming the vehicle maintains a __constant__ altitude.
--
-- @trackPositionAfter ('Track' p b s) d@ is a equivalent to @positionAfter' p ('course' p b) s d@.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Kinematics
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p = s84Pos 53.321 (-1.729) (metres 15000)
-- >>> let b = decimalDegrees 96.0217
-- >>> let s = kilometresPerHour 124.8
-- >>> trackPositionAfter (Track p b s) (hours 1)
-- 53°11'19.368"N,0°8'2.457"E 15.0km (S84)
--
trackPositionAfter :: (Spherical a) => Track a -> Duration -> Geodetic.Position a
trackPositionAfter (Track p b s) = positionAfter' p (course p b) s

-- | @cpa t1 t2@ computes the closest point of approach between tracks @t1@ and @t2@ disregarding
-- their respective altitude.
-- If a closest point of approach is found, height of 'cpaPosition1' - respectively 'cpaPosition2',
-- will be the altitude of the first - respectively second, track.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Kinematics
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let p1 = s84Pos 20 (-60) zero
-- >>> let b1 = decimalDegrees 10
-- >>> let s1 = knots 15
-- >>> let p2 = s84Pos 34 (-50) zero
-- >>> let b2 = decimalDegrees 220
-- >>> let s2 = knots 300
-- >>> let t1 = Track p1 b1 s1
-- >>> let t2 = Track p2 b2 s2
-- >>> let c = cpa t1 t2
-- >>> fmap cpaTime c
-- Just 3H9M56.155S
-- >>> fmap cpaDistance c
-- Just 124.2317453km
--
cpa :: (Spherical a) => Track a -> Track a -> Maybe (Cpa a)
cpa (Track p1 b1 s1) (Track p2 b2 s2)
    | Geodetic.llEq p1 p2 = Just (Cpa Duration.zero Length.zero p1 p2)
    | t < 0 = Nothing
    | otherwise = Just (Cpa (Duration.seconds t) d cp1 cp2)
  where
    c1 = course p1 b1
    c2 = course p2 b2
    t = timeToCpa p1 c1 s1 p2 c2 s2
    cp1 = position' p1 c1 s1 t
    cp2 = position' p2 c2 s2 t
    d = GreatCircle.surfaceDistance cp1 cp2

-- | @intercept t p@ computes the __minimum__ speed of interceptor at
-- position @p@ needed for an intercept with target track @t@ to take place.
-- Intercept time, position, distance and interceptor bearing are derived from
-- this minimum speed. Returns 'Nothing' if intercept cannot be achieved e.g.:
--
--     * interceptor and target are at the same position
--
--     * interceptor is "behind" the target
--
-- If found, 'interceptPosition' is at the altitude of the track.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Kinematics
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let t = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 600)
-- >>> let ip = s84Pos 20 (-60) zero
-- >>> let i = intercept t ip
-- >>> fmap (toKnots . interceptorSpeed) i
-- Just 52.633367756059
-- >>> fmap (toSeconds . interceptTime) i
-- Just 5993.831
--
intercept :: (Spherical a) => Track a -> Geodetic.Position a -> Maybe (Intercept a)
intercept t p = interceptByTime t p (Duration.seconds (timeToIntercept t p))

-- | @interceptBySpeed t p s@ computes the time needed by interceptor at
-- position @p@ and travelling at speed @s@ to intercept target track @t@.
-- Returns 'Nothing' if intercept cannot be achieved e.g.:
--
--     * interceptor and target are at the same position
--
--     * interceptor speed is below minimum speed returned by 'intercept'
--
-- If found, 'interceptPosition' is at the altitude of the track.
--
interceptBySpeed :: (Spherical a) => Track a -> Geodetic.Position a -> Speed -> Maybe (Intercept a)
interceptBySpeed t p s
    | isNothing minInt = Nothing
    | fmap interceptorSpeed minInt == Just s = minInt
    | otherwise = interceptByTime t p (Duration.seconds (timeToInterceptSpeed t p s))
  where
    minInt = intercept t p

-- | @interceptByTime t p d@ computes the speed of interceptor at
-- position @p@ needed for an intercept with target track @t@ to take place
-- after duration @d@. Returns 'Nothing' if given duration is <= 0 or
-- interceptor and target are at the same position.
--
-- If found, 'interceptPosition' is at the altitude of the track.
--
-- Note: contrary to 'intercept' and 'interceptBySpeed' this function handles
-- cases where the interceptor has to catch up the target.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Kinematics
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> let t = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 600)
-- >>> let ip = s84Pos 20 (-60) zero
-- >>> let d = seconds 2700
-- >>> let i = interceptByTime t ip d
-- >>> fmap (toKnots . interceptorSpeed) i
-- Just 730.959238
-- >>>
-- >>> fmap interceptorBearing i
-- Just 26°7'11.649"
-- >>>
-- >>> fmap interceptPosition i
-- Just 28°8'12.047"N,55°27'21.411"W 0.0m (S84)
-- >>>
-- >>> fmap interceptDistance i
-- Just 1015.3023506km
-- >>>
-- >>> fmap (toSeconds . interceptTime) i
-- Just 2700
--
interceptByTime ::
       (Spherical a) => Track a -> Geodetic.Position a -> Duration -> Maybe (Intercept a)
interceptByTime t p d
    | Duration.toMilliseconds d <= 0 = Nothing
    | Geodetic.llEq (trackPosition t) p = Nothing
    | isNothing ib = Nothing
    | otherwise =
        let is = Speed.average idist d
         in Just (Intercept d idist ipos (fromJust ib) is)
  where
    ipos = trackPositionAfter t d
    idist = GreatCircle.surfaceDistance p ipos
    ib = GreatCircle.initialBearing p ipos <|> GreatCircle.initialBearing p (trackPosition t)

-- private
-- | position from speed course and seconds.
position' ::
       (Spherical a) => Geodetic.Position a -> Course -> Speed -> Double -> Geodetic.Position a
position' p0 (Course c) s sec = Geodetic.nvectorHeightPos' v1 h0 (Geodetic.model p0)
  where
    nv0 = Geodetic.nvector p0
    h0 = Geodetic.height p0
    v1 = position'' nv0 c s sec (radiusM p0)

-- | position from course, speed and seconds.
position'' :: V3 -> V3 -> Speed -> Double -> Double -> V3
position'' v0 c s sec rm = v1
  where
    a = Speed.toMetresPerSecond s / rm * sec
    v1 = Math3d.add (Math3d.scale v0 (cos a)) (Math3d.scale c (sin a))

-- | time to CPA.
timeToCpa ::
       (Spherical a)
    => Geodetic.Position a
    -> Course
    -> Speed
    -> Geodetic.Position a
    -> Course
    -> Speed
    -> Double
timeToCpa p1 (Course c10) s1 p2 (Course c20) s2 = cpaNrRec v10 c10 w1 v20 c20 w2 0 0
  where
    v10 = Geodetic.nvector p1
    rm = radiusM p1
    w1 = Speed.toMetresPerSecond s1 / rm
    v20 = Geodetic.nvector p2
    w2 = Speed.toMetresPerSecond s2 / rm

-- | time to intercept with minimum speed
timeToIntercept :: (Spherical a) => Track a -> Geodetic.Position a -> Double
timeToIntercept (Track p2 b2 s2) p1 = intMinNrRec v10v20 v10c2 w2 (sep v10 v20 c2 s2 rm) t0 0
  where
    v10 = Geodetic.nvector p1
    v20 = Geodetic.nvector p2
    (Course c2) = course p2 b2
    v10v20 = Math3d.dot v10 v20
    v10c2 = Math3d.dot v10 c2
    s2mps = Speed.toMetresPerSecond s2
    rm = radiusM p1
    w2 = s2mps / rm
    s0 = Angle.toRadians (Angle.between v10 v20) -- initial angular distance between target and interceptor
    t0 = rm * s0 / s2mps -- assume target is travelling towards interceptor

-- | time to intercept with speed.
timeToInterceptSpeed :: (Spherical a) => Track a -> Geodetic.Position a -> Speed -> Double
timeToInterceptSpeed (Track p2 b2 s2) p1 s1 =
    intSpdNrRec v10v20 v10c2 w1 w2 (sep v10 v20 c2 s2 rm) t0 0
  where
    v10 = Geodetic.nvector p1
    v20 = Geodetic.nvector p2
    (Course c2) = course p2 b2
    v10v20 = Math3d.dot v10 v20
    v10c2 = Math3d.dot v10 c2
    rm = radiusM p1
    w1 = Speed.toMetresPerSecond s1 / rm
    w2 = Speed.toMetresPerSecond s2 / rm
    t0 = 0.1

rx :: Angle -> [V3]
rx a = [V3 1 0 0, V3 0 c s, V3 0 (-s) c]
  where
    c = Angle.cos a
    s = Angle.sin a

ry :: Angle -> [V3]
ry a = [V3 c 0 (-s), V3 0 1 0, V3 s 0 c]
  where
    c = Angle.cos a
    s = Angle.sin a

rz :: Angle -> [V3]
rz a = [V3 c s 0, V3 (-s) c 0, V3 0 0 1]
  where
    c = Angle.cos a
    s = Angle.sin a

cpaA :: V3 -> V3 -> Double -> V3 -> V3 -> Double -> Double
cpaA v10 c10 w1 v20 c20 w2 =
    negate (Math3d.dot (Math3d.scale v10 w1) c20 + Math3d.dot (Math3d.scale v20 w2) c10)

cpaB :: V3 -> V3 -> Double -> V3 -> V3 -> Double -> Double
cpaB v10 c10 w1 v20 c20 w2 =
    Math3d.dot (Math3d.scale c10 w1) v20 + Math3d.dot (Math3d.scale c20 w2) v10

cpaC :: V3 -> V3 -> Double -> V3 -> V3 -> Double -> Double
cpaC v10 c10 w1 v20 c20 w2 =
    negate (Math3d.dot (Math3d.scale v10 w1) v20 - Math3d.dot (Math3d.scale c20 w2) c10)

cpaD :: V3 -> V3 -> Double -> V3 -> V3 -> Double -> Double
cpaD v10 c10 w1 v20 c20 w2 =
    Math3d.dot (Math3d.scale c10 w1) c20 - Math3d.dot (Math3d.scale v20 w2) v10

cpaFt :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
cpaFt cw1t cw2t sw1t sw2t a b c d =
    a * sw1t * sw2t + b * cw1t * cw2t + c * sw1t * cw2t + d * cw1t * sw2t

cpaDft ::
       Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
cpaDft w1 w2 cw1t cw2t sw1t sw2t a b c d =
    negate ((c * w2 + d * w1) * sw1t * sw2t) + (d * w2 + c * w1) * cw1t * cw2t +
    (a * w2 - b * w1) * sw1t * cw2t -
    (b * w2 - a * w1) * cw1t * sw2t

cpaStep :: V3 -> V3 -> Double -> V3 -> V3 -> Double -> Double -> Double
cpaStep v10 c10 w1 v20 c20 w2 t =
    cpaFt cw1t cw2t sw1t sw2t a b c d / cpaDft w1 w2 cw1t cw2t sw1t sw2t a b c d
  where
    cw1t = cos (w1 * t)
    cw2t = cos (w2 * t)
    sw1t = sin (w1 * t)
    sw2t = sin (w2 * t)
    a = cpaA v10 c10 w1 v20 c20 w2
    b = cpaB v10 c10 w1 v20 c20 w2
    c = cpaC v10 c10 w1 v20 c20 w2
    d = cpaD v10 c10 w1 v20 c20 w2

-- | Newton-Raphson for CPA time.
cpaNrRec :: V3 -> V3 -> Double -> V3 -> V3 -> Double -> Double -> Int -> Double
cpaNrRec v10 c10 w1 v20 c20 w2 ti i
    | i == 50 = -1.0 -- no convergence
    | abs fi < 1e-11 = ti1
    | otherwise = cpaNrRec v10 c10 w1 v20 c20 w2 ti1 (i + 1)
  where
    fi = cpaStep v10 c10 w1 v20 c20 w2 ti
    ti1 = ti - fi

-- | Newton-Raphson for min speed intercept.
intMinNrRec :: Double -> Double -> Double -> (Double -> Double) -> Double -> Int -> Double
intMinNrRec v10v20 v10c2 w2 st ti i
    | i == 50 = -1.0 -- no convergence
    | abs fi < 1e-11 = ti1
    | otherwise = intMinNrRec v10v20 v10c2 w2 st ti1 (i + 1)
  where
    cosw2t = cos (w2 * ti)
    sinw2t = sin (w2 * ti)
    v10dv2dt = (-w2) * (v10v20 * sinw2t - v10c2 * cosw2t)
    v10d2v2dt2 = (-1.0 * w2 * w2) * (v10v20 * cosw2t + v10c2 * sinw2t)
    si = st ti
    sinS = sin si
    a = (-1.0) / sinS
    b = cos si / (sinS * sinS)
    f = ti * a * v10dv2dt - si
    d2sdt2 = a * (b * v10dv2dt * v10dv2dt + v10d2v2dt2)
    df = ti * d2sdt2
    fi = f / df
    ti1 = ti - fi

-- | Newton-Raphson for speed intercept.
intSpdNrRec :: Double -> Double -> Double -> Double -> (Double -> Double) -> Double -> Int -> Double
intSpdNrRec v10v20 v10c2 w1 w2 st ti i
    | i == 50 = -1.0 -- no convergence
    | abs fi < 1e-11 = ti1
    | otherwise = intSpdNrRec v10v20 v10c2 w1 w2 st ti1 (i + 1)
  where
    cosw2t = cos (w2 * ti)
    sinw2t = sin (w2 * ti)
    si = st ti
    f = si / ti - w1
    dsdt = (w2 * (v10v20 * sinw2t - v10c2 * cosw2t)) / sin si
    df = (dsdt - (si / ti)) / ti
    fi = f / df
    ti1 = ti - fi

-- | angular separation in radians at ti between v10 and track with initial position v20,
-- course c2 and speed s2.
sep :: V3 -> V3 -> V3 -> Speed -> Double -> Double -> Double
sep v10 v20 c2 s2 r ti = Angle.toRadians (Angle.between v10 (position'' v20 c2 s2 ti r))

-- | reference sphere radius.
radius :: (Spherical a) => Geodetic.Position a -> Length
radius = equatorialRadius . surface . Geodetic.model

-- | reference sphere radius in metres.
radiusM :: (Spherical a) => Geodetic.Position a -> Double
radiusM = Length.toMetres . radius
