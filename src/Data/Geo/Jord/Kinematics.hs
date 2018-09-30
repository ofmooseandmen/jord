{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Data.Geo.Jord.Kinematics
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with kinematics calculations assuming a __spherical__ earth model.
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
    , position
    , position84
    , cpa
    , cpa84
    , intercept
    , intercept84
    , interceptBySpeed
    , interceptBySpeed84
    , interceptByTime
    , interceptByTime84
    ) where

import Control.Applicative
import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Duration
import Data.Geo.Jord.Earth
import Data.Geo.Jord.Geodetics
import Data.Geo.Jord.Internal(ad, nvec)
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Speed
import Data.Geo.Jord.Transformation
import Data.Geo.Jord.Vector3d
import Data.Maybe (isNothing)

-- | 'Track' represents the state of a vehicle by its current position, bearing and speed.
data Track a = Track
    { trackPos :: a -- ^ position of the track.
    , trackBearing :: Angle -- ^ bearing of the track.
    , trackSpeed :: Speed -- ^ speed of the track.
    } deriving (Eq, Show)

-- | 'GreatCircle' from track.
instance NTransform a => IsGreatCircle (Track a) where
    greatCircleE t = greatCircleE (trackPos t, trackBearing t)

-- | 'GreatArc' from track and duration using the mean radius of the WGS84 reference ellipsoid.
instance NTransform a => IsGreatArc (Track a, Duration) where
    greatArcE (t, d) = greatArcE (t, d, r84)

-- | 'GreatArc' from track, duration and earth mean radius.
instance NTransform a => IsGreatArc (Track a, Duration, Length) where
    greatArcE (t, d, r) = greatArcE (trackPos t, position t d r)

-- | 'Course' represents the cardinal direction in which the vehicle is to be steered.
newtype Course =
    Course Vector3d
    deriving (Eq, Show)

instance IsVector3d Course where
    vec (Course v) = v

-- | Time to, and distance at, closest point of approach (CPA) as well as position of both tracks at CPA.
data Cpa a = Cpa
    { cpaTime :: Duration -- ^ time to CPA.
    , cpaDistance :: Length -- ^ distance at CPA.
    , cpaPosition1 :: a -- ^ position of track 1 at CPA.
    , cpaPosition2 :: a -- ^ position of track 2 at CPA.
    } deriving (Eq, Show)

-- | Time, distance and position of intercept as well as speed and initial bearing of interceptor.
data Intercept a = Intercept
    { interceptTime :: Duration -- ^ time to intercept.
    , interceptDistance :: Length -- ^ distance at intercept.
    , interceptPosition :: a -- ^ position of intercept.
    , interceptorBearing :: Angle -- ^ initial bearing of interceptor.
    , interceptorSpeed :: Speed -- ^ speed of interceptor.
    } deriving (Eq, Show)

-- | @course p b@ computes the course of a vehicle currently at position @p@ and following bearing @b@.
course :: (NTransform a) => a -> Angle -> Course
course p b = Course (Vector3d (vz (head r)) (vz (r !! 1)) (vz (r !! 2)))
  where
    ll = nvectorToLatLong . pos . toNVector $ p
    lat = latitude ll
    lon = longitude ll
    r = mdot (mdot (rz (negate' lon)) (ry lat)) (rx b)

-- | @position t d r@ computes the position of a track @t@ after duration @d@ has elapsed and using the earth radius @r@.
--
-- @
--     let p0 = latLongHeight (readLatLong "531914N0014347W") (metres 15000)
--     let b = decimalDegrees 96.0217
--     let s = kilometresPerHour 124.8
--     let p1 = decimalLatLongHeight 53.1882691 0.1332741 (metres 15000)
--     position (Track p0 b s) (hours 1) r84 = p1
-- @
position :: (NTransform a) => Track a -> Duration -> Length -> a
position (Track p0 b s) d = position' p0 s (course p0 b) (toSeconds d)

-- | 'position' using the mean radius of the WGS84 reference ellipsoid.
position84 :: (NTransform a) => Track a -> Duration -> a
position84 t d = position t d r84

-- | @cpa t1 t2 r@ computes the closest point of approach between tracks @t1@ and @t2@ and using the earth radius @r@.
--
-- @
--     let p1 = decimalLatLong 20 (-60)
--     let b1 = decimalDegrees 10
--     let s1 = knots 15
--     let p2 = decimalLatLong 34 (-50)
--     let b2 = decimalDegrees 220
--     let s2 = knots 300
--     let t1 = Track p1 b1 s1
--     let t2 = Track p2 b2 s2
--     let c = cpa t1 t2 r84
--     fmap cpaTime c = Just (milliseconds 11396155)
--     fmap cpaDistance c = Just (kilometres 124.2317453)
-- @
cpa :: (Eq a, NTransform a) => Track a -> Track a -> Length -> Maybe (Cpa a)
cpa (Track p1 b1 s1) (Track p2 b2 s2) r
    | p1 == p2 = Just (Cpa zero zero p1 p2)
    | t < 0 = Nothing
    | otherwise = Just (Cpa (seconds t) d cp1 cp2)
  where
    c1 = course p1 b1
    c2 = course p2 b2
    t = timeToCpa p1 c1 s1 p2 c2 s2 r
    cp1 = position' p1 s1 c1 t r
    cp2 = position' p2 s2 c2 t r
    d = surfaceDistance cp1 cp2 r

-- | 'cpa' using the mean radius of the WGS84 reference ellipsoid.
cpa84 :: (Eq a, NTransform a) => Track a -> Track a -> Maybe (Cpa a)
cpa84 t1 t2 = cpa t1 t2 r84

-- | @intercept t p r@ computes the __minimum__ speed of interceptor at
-- position @p@ needed for an intercept with target track @t@ to take place
-- using the earth radius @r@. Intercept time, position, distance and interceptor
-- bearing are derived from this minimum speed. Returns 'Nothing' if intercept
-- cannot be achieved e.g.:
--
--     * interceptor and target are at the same position
--
--     * interceptor is "behind" the target
--
-- @
--     let t = Track (decimalLatLong 34 (-50)) (decimalDegrees 220) (knots 600)
--     let ip = (decimalLatLong 20 (-60))
--     let i = intercept t ip r84
--     fmap interceptorSpeed i = Just (knots 52.633367756059)
--     fmap interceptTime i = Just (seconds 5993.831)
-- @
intercept :: (Eq a, NTransform a) => Track a -> a -> Length -> Maybe (Intercept a)
intercept t p r = interceptByTime t p (seconds (timeToIntercept t p r)) r

-- | 'intercept' using the mean radius of the WGS84 reference ellipsoid.
intercept84 :: (Eq a, NTransform a) => Track a -> a -> Maybe (Intercept a)
intercept84 t p = intercept t p r84

-- | @interceptBySpeed t p s r@ computes the time needed by interceptor at
-- position @p@ and travelling at speed @s@ to intercept target track @t@
-- using the earth radius @r@. Returns 'Nothing' if intercept
-- cannot be achieved e.g.:
--
--     * interceptor and target are at the same position
--
--     * interceptor speed is below minimum speed returned by 'intercept'
interceptBySpeed :: (Eq a, NTransform a) => Track a -> a -> Speed -> Length -> Maybe (Intercept a)
interceptBySpeed t p s r
    | isNothing minInt = Nothing
    | fmap interceptorSpeed minInt == Just s = minInt
    | otherwise = interceptByTime t p (seconds (timeToInterceptSpeed t p s r)) r
  where
    minInt = intercept t p r

-- | 'interceptBySpeed' using the mean radius of the WGS84 reference ellipsoid.
interceptBySpeed84 :: (Eq a, NTransform a) => Track a -> a -> Speed -> Maybe (Intercept a)
interceptBySpeed84 t p s = interceptBySpeed t p s r84

-- | @interceptByTime t p d r@ computes the speed of interceptor at
-- position @p@ needed for an intercept with target track @t@ to take place
-- after duration @d@ and using the earth radius @r@. Returns 'Nothing' if
-- given duration is <= 0 or interceptor and target are at the same position.
--
-- @
--     let t = Track (decimalLatLong 34 (-50)) (decimalDegrees 220) (knots 600)
--     let ip = (decimalLatLong 20 (-60))
--     let d = seconds 2700
--     let i = interceptByTime t ip d r84
--     fmap interceptorSpeed i = Just (knots 730.959238)
--     fmap interceptorBearing i = Just (decimalDegrees 26.1199030)
--     fmap interceptPosition i = Just (decimalLatLong 28.1366797 (-55.4559475))
--     fmap interceptDistance i = Just (metres 1015302.3815)
--     fmap interceptTime i = Just (seconds 2700)
-- @
--
-- Note: contrary to 'intercept' and 'interceptBySpeed' this function handles
-- cases where the interceptor has to catch up the target.
interceptByTime :: (Eq a, NTransform a) => Track a -> a -> Duration -> Length -> Maybe (Intercept a)
interceptByTime t p d r
    | toMilliseconds d <= 0 = Nothing
    | trackPos t == p = Nothing
    | otherwise = fmap (\b -> Intercept d idist ipos b is) ib
  where
    ipos = position t d r
    idist = surfaceDistance p ipos r
    ib = initialBearing p ipos <|> initialBearing p (trackPos t)
    is = metresPerSecond (toMetres idist / toSeconds d)

-- | 'interceptByTime' using the mean radius of the WGS84 reference ellipsoid.
interceptByTime84 :: (Eq a, NTransform a) => Track a -> a -> Duration -> Maybe (Intercept a)
interceptByTime84 t p d = interceptByTime t p d r84

-- | position from speed course and seconds.
position' :: (NTransform a) => a -> Speed -> Course -> Double -> Length -> a
position' p0 s c sec r = fromNVector (nvectorHeight (nvector (vx v1) (vy v1) (vz v1)) h0)
  where
    nv0 = toNVector p0
    v0 = vec . pos $nv0
    h0 = height nv0
    v1 = position'' v0 s (vec c) sec r

-- | position from speed course and seconds.
position'' :: Vector3d -> Speed -> Vector3d -> Double -> Length -> Vector3d
position'' v0 s c sec r = v1
  where
    a = toMetresPerSecond s / toMetres r * sec
    v1 = vadd (vscale v0 (cos a)) (vscale c (sin a))

-- | time to CPA.
timeToCpa :: (NTransform a) => a -> Course -> Speed -> a -> Course -> Speed -> Length -> Double
timeToCpa p1 c1 s1 p2 c2 s2 r = cpaNrRec v10 c10 w1 v20 c20 w2 0 0
  where
    v10 = nvec p1
    c10 = vec c1
    rm = toMetres r
    w1 = toMetresPerSecond s1 / rm
    v20 = nvec p2
    c20 = vec c2
    w2 = toMetresPerSecond s2 / rm

-- | time to intercept with minimum speed
timeToIntercept :: (NTransform a) => Track a -> a -> Length -> Double
timeToIntercept (Track p2 b2 s2) p1 r = intMinNrRec v10v20 v10c2 w2 (sep v10 v20 c2 s2 r) t0 0
  where
    v10 = nvec p1
    v20 = nvec p2
    c2 = vec (course p2 b2)
    v10v20 = vdot v10 v20
    v10c2 = vdot v10 c2
    s2mps = toMetresPerSecond s2
    rm = toMetres r
    w2 = s2mps / rm
    s0 = ad v10 v20 -- initial angular distance between target and interceptor
    t0 = rm * s0 / s2mps -- assume target is travelling towards interceptor

-- | time to intercept with speed.
timeToInterceptSpeed :: (NTransform a) => Track a -> a -> Speed -> Length -> Double
timeToInterceptSpeed (Track p2 b2 s2) p1 s1 r =
    intSpdNrRec v10v20 v10c2 w1 w2 (sep v10 v20 c2 s2 r) t0 0
  where
    v10 = nvec p1
    v20 = nvec p2
    c2 = vec (course p2 b2)
    v10v20 = vdot v10 v20
    v10c2 = vdot v10 c2
    rm = toMetres r
    w1 = toMetresPerSecond s1 / rm
    w2 = toMetresPerSecond s2 / rm
    t0 = 0.1

rx :: Angle -> [Vector3d]
rx a = [Vector3d 1 0 0, Vector3d 0 c s, Vector3d 0 (-s) c]
  where
    c = cos' a
    s = sin' a

ry :: Angle -> [Vector3d]
ry a = [Vector3d c 0 (-s), Vector3d 0 1 0, Vector3d s 0 c]
  where
    c = cos' a
    s = sin' a

rz :: Angle -> [Vector3d]
rz a = [Vector3d c s 0, Vector3d (-s) c 0, Vector3d 0 0 1]
  where
    c = cos' a
    s = sin' a

cpaA :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
cpaA v10 c10 w1 v20 c20 w2 = negate (vdot (vscale v10 w1) c20 + vdot (vscale v20 w2) c10)

cpaB :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
cpaB v10 c10 w1 v20 c20 w2 = vdot (vscale c10 w1) v20 + vdot (vscale c20 w2) v10

cpaC :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
cpaC v10 c10 w1 v20 c20 w2 = negate (vdot (vscale v10 w1) v20 - vdot (vscale c20 w2) c10)

cpaD :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
cpaD v10 c10 w1 v20 c20 w2 = vdot (vscale c10 w1) c20 - vdot (vscale v20 w2) v10

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

cpaStep :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double -> Double
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
       Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double -> Int -> Double
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
sep :: Vector3d -> Vector3d -> Vector3d -> Speed -> Length -> Double -> Double
sep v10 v20 c2 s2 r ti = ad v10 (position'' v20 s2 c2 ti r)
