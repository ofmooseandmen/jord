-- |
-- Module:      Data.Geo.Jord.kinematics
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
-- and in <https://calhoun.nps.edu/bitstream/handle/10945/29516/sometacticalalgo00shud.pdf Some tactical algorithms for spherical geometry>
--
module Data.Geo.Jord.Kinematics
    (
    -- * The 'Track' type.
      Track(..)
    -- * The 'Course' type.
    , Course
    -- * The 'Cpa' type.
    , Cpa(cpaTime, cpaDistance, cpaPos1, cpaPos2)
    -- * Calculations
    , course
    , position
    , position84
    , cpa
    , cpa84
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Duration
import Data.Geo.Jord.Earth
import Data.Geo.Jord.Geodetics
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Speed
import Data.Geo.Jord.Transform
import Data.Geo.Jord.Vector3d

-- | 'Track' represents the state of a vehicle by its current position, bearing and speed.
data Track a = Track
    { trackPos :: a
    , trackBearing :: Angle
    , trackSpeed :: Speed
    } deriving (Eq, Show)

-- | 'Course' represents the cardinal direction in which the vehicle is to be steered.
newtype Course =
    Course Vector3d
    deriving (Eq, Show)

instance IsVector3d Course where
    vec (Course v) = v

-- | Time to, and distance at, Closest point of approach as well as position of both track at CPA.
data Cpa a = Cpa
    { cpaTime :: Duration -- ^ time to CPA
    , cpaDistance :: Length -- ^ distance at CPA
    , cpaPos1 :: a -- position of track 1 at CPA
    , cpaPos2 :: a -- position of track 2 at CPA
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
--     position (Track p0 b s) (hours 1) = p1
-- @
position :: (NTransform a) => Track a -> Duration -> Length -> a
position (Track p0 b s) d = positionC p0 s (course p0 b) (toSeconds d)

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
--     let c = cpa84 t1 t2
--     fmap cpaTime c = Just (milliseconds 11396155)
--     fmap cpaDistance c = Just (kilometres 124.2317453)
-- @
cpa :: (NTransform a) => Track a -> Track a -> Length -> Maybe (Cpa a)
cpa (Track p1 b1 s1) (Track p2 b2 s2) r
    | t < 0 = Nothing
    | otherwise = Just (Cpa (seconds t) d cp1 cp2)
  where
    c1 = course p1 b1
    c2 = course p2 b2
    t = timeToCpa p1 c1 s1 p2 c2 s2
    cp1 = positionC p1 s1 c1 t r
    cp2 = positionC p2 s2 c2 t r
    d = surfaceDistance cp1 cp2 r

-- | 'cpa' using the mean radius of the WGS84 reference ellipsoid.
cpa84 :: (NTransform a) => Track a -> Track a -> Maybe (Cpa a)
cpa84 t1 t2 = cpa t1 t2 r84

-- | position from speed and course.
positionC :: (NTransform a) => a -> Speed -> Course -> Double -> Length -> a
positionC p0 s c sec r = fromNVector (nvectorHeight (nvector (vx v1) (vy v1) (vz v1)) h0)
  where
    nv0 = toNVector p0
    v0 = vec . pos $ nv0
    h0 = height nv0
    w = toMetresPerSecond s / toMetres r
    vc = vec c
    v1 = vadd (vscale v0 (cos (w * sec))) (vscale vc (sin (w * sec)))

-- | time to CPA.
timeToCpa :: (NTransform a) => a -> Course -> Speed -> a -> Course -> Speed -> Double
timeToCpa p1 c1 s1 p2 c2 s2 = cpaNr v10 c10 w1 v20 c20 w2
  where
    v10 = vec . pos . toNVector $ p1
    c10 = vec c1
    w1 = toMetresPerSecond s1 / toMetres r84
    v20 = vec . pos . toNVector $ p2
    c20 = vec c2
    w2 = toMetresPerSecond s2 / toMetres r84

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

cpaDft :: Double
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

cpaNr :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
cpaNr v10 c10 w1 v20 c20 w2 = cpaNrRec v10 c10 w1 v20 c20 w2 0 0

cpaNrRec :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double -> Int -> Double
cpaNrRec v10 c10 w1 v20 c20 w2 ti i
    | i == 50 = ti1
    | abs fi < 1e-12 = ti1
    | otherwise = cpaNrRec v10 c10 w1 v20 c20 w2 ti1 (i + 1)
  where
    fi = cpaStep v10 c10 w1 v20 c20 w2 ti
    ti1 = ti - fi
