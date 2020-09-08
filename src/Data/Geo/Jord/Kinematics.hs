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
-- import qualified Data.Geo.Jord.Geodetic as Geodetic
-- import qualified Data.Geo.Jord.Kinematics as Kinematics
-- @
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
-- and in <https://calhoun.nps.edu/bitstream/handle/10945/29516/sometacticalalgo00shud.pdf Shudde, Rex H. (1986). Some tactical algorithms for spherical geometry>
--
module Data.Geo.Jord.Kinematics
    (
    -- * The 'Track' type.
      Track(..)
    -- * The 'Course' type.
    , Course
    -- * The 'Cpa' type.
    , Cpa
    , timeToCpa
    , distanceAtCpa
    , cpaOwnshipPosition
    , cpaIntruderPosition
    -- * The 'Intercept' type.
    , Intercept
    , timeToIntercept
    , distanceToIntercept
    , interceptPosition
    , interceptorBearing
    , interceptorSpeed
    -- * Calculations
    , course
    , positionAfter
    , positionAfter'
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
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle (distance, initialBearing)
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (toMetres, zero)
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Spherical, surface)
import Data.Geo.Jord.Speed (Speed)
import qualified Data.Geo.Jord.Speed as Speed (average, toMetresPerSecond)

-- | 'Track' represents the state of a vehicle by its current horizontal position, bearing and speed.
data Track a =
    Track
        { trackPosition :: HorizontalPosition a -- ^ horizontal position of the track.
        , trackBearing :: Angle -- ^ bearing of the track.
        , trackSpeed :: Speed -- ^ speed of the track.
        }
    deriving (Eq, Show)

-- | 'Course' represents the cardinal direction in which the vehicle is to be steered.
newtype Course =
    Course Math3d.V3
    deriving (Eq, Show)

-- | Time to, and distance at, closest point of approach (CPA) as well as position of both tracks at CPA.
data Cpa a =
    Cpa
        { timeToCpa :: Duration -- ^ time to CPA.
        , distanceAtCpa :: Length -- ^ distance at CPA.
        , cpaOwnshipPosition :: HorizontalPosition a -- ^ horizontal position of ownship CPA.
        , cpaIntruderPosition :: HorizontalPosition a -- ^ horizontal position of intruder at CPA.
        }
    deriving (Eq, Show)

-- | Time, distance and position of intercept as well as speed and initial bearing of interceptor.
data Intercept a =
    Intercept
        { timeToIntercept :: Duration -- ^ time to intercept.
        , distanceToIntercept :: Length -- ^ distance travelled to intercept.
        , interceptPosition :: HorizontalPosition a -- ^ horizontal position of intercept.
        , interceptorBearing :: Angle -- ^ initial bearing of interceptor.
        , interceptorSpeed :: Speed -- ^ speed of interceptor.
        }
    deriving (Eq, Show)

-- | @course p b@ computes the course of a vehicle currently at position @p@ and following bearing @b@.
course :: (Spherical a) => HorizontalPosition a -> Angle -> Course
course p b = Course (Math3d.vec3 (Math3d.v3z (head r)) (Math3d.v3z (r !! 1)) (Math3d.v3z (r !! 2)))
  where
    lat = Geodetic.latitude p
    lon = Geodetic.longitude p
    r = Math3d.dotM (Math3d.dotM (rz (Angle.negate lon)) (ry lat)) (rx b)

-- | @positionAfter p b s d@ computes the horizontal position of a vehicle currently at position @p@ following
-- bearing @b@ and travelling at speed @s@ after duration @d@ has elapsed. For example:
--
-- >>> let p = Geodetic.s84Pos 53.321 (-1.729)
-- >>> let b = Angle.decimalDegrees 96.0217
-- >>> let s = Speed.kilometresPerHour 124.8
-- >>> Kinematics.positionAfter p b s (Duration.hours 1)
-- 53°11'19.367"N,0°8'2.456"E (S84)
--
-- This is equivalent to:
--
-- > Kinematics.positionAfter' p (Kinematics.course p b) s d
positionAfter ::
       (Spherical a) => HorizontalPosition a -> Angle -> Speed -> Duration -> HorizontalPosition a
positionAfter p b s d = position' p (course p b) s (Duration.toSeconds d)

-- | @positionAfter' p c s d@ computes the horizontal position of a vehicle currently at position @p@ on course @c@ and
-- travelling at speed @s@ after duration @d@ has elapsed.
-- Note: course must have been calculated from position @p@.
positionAfter' ::
       (Spherical a) => HorizontalPosition a -> Course -> Speed -> Duration -> HorizontalPosition a
positionAfter' p c s d = position' p c s (Duration.toSeconds d)

-- | @trackPositionAfter t d@ computes the horizontal position of a track @t@ after duration @d@ has elapsed. For example:
--
-- >>> let p = Geodetic.s84Pos 53.321 (-1.729)
-- >>> let b = Angle.decimalDegrees 96.0217
-- >>> let s = Speed.kilometresPerHour 124.8
-- >>> Kinematics.trackPositionAfter (Kinematics.Track p b s) (Duration.hours 1)
-- 53°11'19.367"N,0°8'2.456"E (S84)
trackPositionAfter :: (Spherical a) => Track a -> Duration -> HorizontalPosition a
trackPositionAfter (Track p b s) = positionAfter' p (course p b) s

-- | @cpa ownship intruder@ computes the closest point of approach between tracks @ownship@ and @intruder@.
-- The closest point of approach is calculated assuming both ships maintain a constant course and speed.
--
-- >>> let ownship = Kinematics.Track (Geodetic.s84Pos 20 (-60)) (Angle.decimalDegrees 10) (Speed.knots 15)
-- >>> let intruder = Kinematics.Track (Geodetic.s84Pos 34 (-50)) (Angle.decimalDegrees 220) (Speed.knots 300)
-- >>> let cpa = Kinematics.cpa ownship intruder
-- Just (Cpa { timeToCpa = 3H9M56.155S
--           , distanceAtCpa = 124.231730834km
--           , cpaOwnshipPosition = 20°46'43.641"N,59°51'11.225"W (S84)
--           , cpaIntruderPosition = 21°24'8.523"N,60°50'48.159"W (S84)})
cpa :: (Spherical a) => Track a -> Track a -> Maybe (Cpa a)
cpa (Track p1 b1 s1) (Track p2 b2 s2)
    | p1 == p2 = Just (Cpa Duration.zero Length.zero p1 p2)
    | t < 0 = Nothing
    | otherwise = Just (Cpa (Duration.seconds t) d cp1 cp2)
  where
    c1 = course p1 b1
    c2 = course p2 b2
    t = timeToCpa' p1 c1 s1 p2 c2 s2
    cp1 = position' p1 c1 s1 t
    cp2 = position' p2 c2 s2 t
    d = GreatCircle.distance cp1 cp2

-- | @intercept t p@ computes the __minimum__ speed of interceptor at position @p@ needed for an intercept with target
-- track @t@ to take place. Intercept time, position, distance and interceptor bearing are derived from this minimum
-- speed. For example:
--
-- >>> let t = Kinematics.Track (Geodetic.s84Pos 34 (-50)) (Angle.decimalDegrees 220) (Speed.knots 600)
-- >>> let ip = Geodetic.s84Pos 20 (-60)
-- >>> Kinematics.intercept t ip
-- Just (Intercept { timeToIntercept = 1H39M53.831S
--                 , distanceToIntercept = 162.294627463km
--                 , interceptPosition = 20°43'42.305"N,61°20'56.848"W (S84)
--                 , interceptorBearing = 300°10'18.053"
--                 , interceptorSpeed = 97.476999km/h})
--
-- Returns 'Nothing' if intercept cannot be achieved e.g.:
--
--     * interceptor and target are at the same position
--
--     * interceptor is "behind" the target
--
intercept :: (Spherical a) => Track a -> HorizontalPosition a -> Maybe (Intercept a)
intercept t p = interceptByTime t p (Duration.seconds (timeToIntercept' t p))

-- | @interceptBySpeed t p s@ computes the time needed by interceptor at position @p@ and travelling at speed @s@ to
-- intercept target track @t@.
--
-- Returns 'Nothing' if intercept cannot be achieved e.g.:
--
--     * interceptor and target are at the same position
--
--     * interceptor speed is below minimum speed returned by 'intercept'
interceptBySpeed :: (Spherical a) => Track a -> HorizontalPosition a -> Speed -> Maybe (Intercept a)
interceptBySpeed t p s
    | isNothing minInt = Nothing
    | fmap interceptorSpeed minInt == Just s = minInt
    | otherwise = interceptByTime t p (Duration.seconds (timeToInterceptSpeed t p s))
  where
    minInt = intercept t p

-- | @interceptByTime t p d@ computes the speed of interceptor at position @p@ needed for an intercept with target
-- track @t@ to take place after duration @d@.For example:
--
-- >>> let t = Kinematics.Track (Geodetic.s84Pos 34 (-50)) (Angle.decimalDegrees 220) (Speed.knots 600)
-- >>> let ip = Geodetic.s84Pos 20 (-60)
-- >>> let d = Duration.seconds 2700
-- >>> interceptByTime t ip d
-- Just (Intercept { timeToIntercept = 0H45M0.000S
--                 , distanceToIntercept = 1015.302358852km
--                 , interceptPosition = 28°8'12.046"N,55°27'21.411"W (S84)
--                 , interceptorBearing = 26°7'11.649"
--                 , interceptorSpeed = 1353.736478km/h})
--
-- Returns 'Nothing' if given duration is <= 0 or interceptor and target are at the same position. Contrary to
-- 'intercept' and 'interceptBySpeed' this function handles cases where the interceptor has to catch up the target.
interceptByTime ::
       (Spherical a) => Track a -> HorizontalPosition a -> Duration -> Maybe (Intercept a)
interceptByTime t p d
    | Duration.toMilliseconds d <= 0 = Nothing
    | trackPosition t == p = Nothing
    | isNothing ib = Nothing
    | otherwise =
        let is = Speed.average idist d
         in Just (Intercept d idist ipos (fromJust ib) is)
  where
    ipos = trackPositionAfter t d
    idist = GreatCircle.distance p ipos
    ib = GreatCircle.initialBearing p ipos <|> GreatCircle.initialBearing p (trackPosition t)

-- private
-- | position from speed course and seconds.
position' ::
       (Spherical a) => HorizontalPosition a -> Course -> Speed -> Double -> HorizontalPosition a
position' p0 (Course c) s sec = Geodetic.nvectorPos' v1 (Geodetic.model p0)
  where
    nv0 = Geodetic.nvector p0
    v1 = position'' nv0 c s sec (radiusM p0)

-- | position from course, speed and seconds.
position'' :: Math3d.V3 -> Math3d.V3 -> Speed -> Double -> Double -> Math3d.V3
position'' v0 c s sec rm = v1
  where
    a = Speed.toMetresPerSecond s / rm * sec
    v1 = Math3d.add (Math3d.scale v0 (cos a)) (Math3d.scale c (sin a))

-- | time to CPA.
timeToCpa' ::
       (Spherical a)
    => HorizontalPosition a
    -> Course
    -> Speed
    -> HorizontalPosition a
    -> Course
    -> Speed
    -> Double
timeToCpa' p1 (Course c10) s1 p2 (Course c20) s2 = cpaNrRec v10 c10 w1 v20 c20 w2 0 0
  where
    v10 = Geodetic.nvector p1
    rm = radiusM p1
    w1 = Speed.toMetresPerSecond s1 / rm
    v20 = Geodetic.nvector p2
    w2 = Speed.toMetresPerSecond s2 / rm

-- | time to intercept with minimum speed
timeToIntercept' :: (Spherical a) => Track a -> HorizontalPosition a -> Double
timeToIntercept' (Track p2 b2 s2) p1 = intMinNrRec v10v20 v10c2 w2 (sep v10 v20 c2 s2 rm) t0 0
  where
    v10 = Geodetic.nvector p1
    v20 = Geodetic.nvector p2
    (Course c2) = course p2 b2
    v10v20 = Math3d.dot v10 v20
    v10c2 = Math3d.dot v10 c2
    s2mps = Speed.toMetresPerSecond s2
    rm = radiusM p1
    w2 = s2mps / rm
    s0 = angleBetweenRadians v10 v20 -- initial angular distance between target and interceptor
    t0 = rm * s0 / s2mps -- assume target is travelling towards interceptor

-- | time to intercept with speed.
timeToInterceptSpeed :: (Spherical a) => Track a -> HorizontalPosition a -> Speed -> Double
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

rx :: Angle -> [Math3d.V3]
rx a = [Math3d.vec3 1 0 0, Math3d.vec3 0 c s, Math3d.vec3 0 (-s) c]
  where
    c = Angle.cos a
    s = Angle.sin a

ry :: Angle -> [Math3d.V3]
ry a = [Math3d.vec3 c 0 (-s), Math3d.vec3 0 1 0, Math3d.vec3 s 0 c]
  where
    c = Angle.cos a
    s = Angle.sin a

rz :: Angle -> [Math3d.V3]
rz a = [Math3d.vec3 c s 0, Math3d.vec3 (-s) c 0, Math3d.vec3 0 0 1]
  where
    c = Angle.cos a
    s = Angle.sin a

cpaA :: Math3d.V3 -> Math3d.V3 -> Double -> Math3d.V3 -> Math3d.V3 -> Double -> Double
cpaA v10 c10 w1 v20 c20 w2 =
    negate (Math3d.dot (Math3d.scale v10 w1) c20 + Math3d.dot (Math3d.scale v20 w2) c10)

cpaB :: Math3d.V3 -> Math3d.V3 -> Double -> Math3d.V3 -> Math3d.V3 -> Double -> Double
cpaB v10 c10 w1 v20 c20 w2 =
    Math3d.dot (Math3d.scale c10 w1) v20 + Math3d.dot (Math3d.scale c20 w2) v10

cpaC :: Math3d.V3 -> Math3d.V3 -> Double -> Math3d.V3 -> Math3d.V3 -> Double -> Double
cpaC v10 c10 w1 v20 c20 w2 =
    negate (Math3d.dot (Math3d.scale v10 w1) v20 - Math3d.dot (Math3d.scale c20 w2) c10)

cpaD :: Math3d.V3 -> Math3d.V3 -> Double -> Math3d.V3 -> Math3d.V3 -> Double -> Double
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

cpaStep :: Math3d.V3 -> Math3d.V3 -> Double -> Math3d.V3 -> Math3d.V3 -> Double -> Double -> Double
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
cpaNrRec ::
       Math3d.V3
    -> Math3d.V3
    -> Double
    -> Math3d.V3
    -> Math3d.V3
    -> Double
    -> Double
    -> Int
    -> Double
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
sep :: Math3d.V3 -> Math3d.V3 -> Math3d.V3 -> Speed -> Double -> Double -> Double
sep v10 v20 c2 s2 r ti = angleBetweenRadians v10 (position'' v20 c2 s2 ti r)

-- | reference sphere radius.
radius :: (Spherical a) => HorizontalPosition a -> Length
radius = equatorialRadius . surface . Geodetic.model

-- | reference sphere radius in metres.
radiusM :: (Spherical a) => HorizontalPosition a -> Double
radiusM = Length.toMetres . radius

-- angle between 2 vectors in radians - this is duplicated with GreatCircle but
-- does not return an Angle - truncating to microarcsecond resolution can be
-- detrimental to the convergence of Newtow-Raphson.
angleBetweenRadians :: Math3d.V3 -> Math3d.V3 -> Double
angleBetweenRadians v1 v2 = atan2 sinO cosO
  where
    sinO = Math3d.norm (Math3d.cross v1 v2)
    cosO = Math3d.dot v1 v2
