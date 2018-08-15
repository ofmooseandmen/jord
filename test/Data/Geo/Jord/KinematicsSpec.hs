module Data.Geo.Jord.KinematicsSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

-- | see <https://calhoun.nps.edu/bitstream/handle/10945/29516/sometacticalalgo00shud.pdf?sequence=1&isAllowed=y Some tactical algorithms for spherical geometry>
spec :: Spec
spec =
    describe "kinematics" $ do
        it "computes position at t from p0, bearing and speed" $ do
            let p0 = latLongHeight (readLatLong "531914N0014347W") (metres 15000)
            let b = decimalDegrees 96.0217
            let s = kilometresPerHour 124.8
            let p1 = decimalLatLongHeight 53.1882691 0.1332741 (metres 15000)
            position84 p0 b s 3600 `shouldBe` p1
        it "computes time to CPA, positions and distance at CPA" $ do
            let p1 = decimalLatLong 20 (-60)
            let b1 = decimalDegrees 10
            let s1 = knots 15
            let p2 = decimalLatLong 34 (-50)
            let b2 = decimalDegrees 220
            let s2 = knots 300
            let t = round (cpaTime p1 b1 s1 p2 b2 s2) :: Int -- seconds
            t `shouldBe` 11396 -- 3h09m55s
            let cpaP1 = position84 p1 b1 s1 t
            let cpaP2 = position84 p2 b2 s2 t
            let d = surfaceDistance84 cpaP1 cpaP2
            d `shouldBe` nauticalMiles 67.07977591

position :: (NTransform a) => a -> Angle -> Speed -> Int -> Length -> a
position p0 b s d r = fromNVector (nvectorHeight (nvector (vx v1) (vy v1) (vz v1)) h0)
  where
    nv0 = toNVector p0
    v0 = vec . pos $ nv0
    h0 = height nv0
    w = toMetresPerSecond s / toMetres r
    c = course p0 b
    sec = fromIntegral d
    v1 = vadd (vscale v0 (cos (w * sec))) (vscale c (sin (w * sec)))

position84 :: (NTransform a) => a -> Angle -> Speed -> Int -> a
position84 p0 b s d = position p0 b s d r84

cpaTime :: (NTransform a) => a -> Angle -> Speed -> a -> Angle -> Speed -> Double
cpaTime p1 b1 s1 p2 b2 s2 = nr v10 c10 w1 v20 c20 w2
  where
    v10 = vec . pos . toNVector $ p1
    c10 = course p1 b1
    w1 = toMetresPerSecond s1 / toMetres r84
    v20 = vec . pos . toNVector $ p2
    c20 = course p2 b2
    w2 = toMetresPerSecond s2 / toMetres r84

course :: (NTransform a) => a -> Angle -> Vector3d
course p b = Vector3d (vz (head r)) (vz (r !! 1)) (vz (r !! 2))
  where
    ll = nvectorToLatLong . pos . toNVector $ p
    lat = latitude ll
    lon = longitude ll
    r = mdot (mdot (rz (negate' lon)) (ry lat)) (rx b)

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

a' :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
a' v10 c10 w1 v20 c20 w2 = negate (vdot (vscale v10 w1) c20 + vdot (vscale v20 w2) c10)

b' :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
b' v10 c10 w1 v20 c20 w2 = vdot (vscale c10 w1) v20 + vdot (vscale c20 w2) v10

c' :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
c' v10 c10 w1 v20 c20 w2 = negate (vdot (vscale v10 w1) v20 - vdot (vscale c20 w2) c10)

d' :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
d' v10 c10 w1 v20 c20 w2 = vdot (vscale c10 w1) c20 - vdot (vscale v20 w2) v10

ft :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double -> Double
ft v10 c10 w1 v20 c20 w2 t = a * sw1t * sw2t + b * cw1t * cw2t + c * sw1t * cw2t + d * cw1t * sw2t
  where
    cw1t = cos (w1 * t)
    cw2t = cos (w2 * t)
    sw1t = sin (w1 * t)
    sw2t = sin (w2 * t)
    a = a' v10 c10 w1 v20 c20 w2
    b = b' v10 c10 w1 v20 c20 w2
    c = c' v10 c10 w1 v20 c20 w2
    d = d' v10 c10 w1 v20 c20 w2

ft' :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double -> Double
ft' v10 c10 w1 v20 c20 w2 t =
    negate ((c * w2 + d * w1) * sw1t * sw2t) + (d * w2 + c * w1) * cw1t * cw2t +
    (a * w2 - b * w1) * sw1t * cw2t -
    (b * w2 - a * w1) * cw1t * sw2t
  where
    cw1t = cos (w1 * t)
    cw2t = cos (w2 * t)
    sw1t = sin (w1 * t)
    sw2t = sin (w2 * t)
    a = a' v10 c10 w1 v20 c20 w2
    b = b' v10 c10 w1 v20 c20 w2
    c = c' v10 c10 w1 v20 c20 w2
    d = d' v10 c10 w1 v20 c20 w2

step :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double -> Double
step v10 c10 w1 v20 c20 w2 t = ft v10 c10 w1 v20 c20 w2 t / ft' v10 c10 w1 v20 c20 w2 t

nr :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double
nr v10 c10 w1 v20 c20 w2 = nr' v10 c10 w1 v20 c20 w2 60 0

nr' :: Vector3d -> Vector3d -> Double -> Vector3d -> Vector3d -> Double -> Double -> Int -> Double
nr' v10 c10 w1 v20 c20 w2 ti i
    | i == 50 = ti1
    | abs fi < 1e-12 = ti1
    | otherwise = nr' v10 c10 w1 v20 c20 w2 ti1 (i + 1)
  where
    fi = step v10 c10 w1 v20 c20 w2 ti
    ti1 = ti - fi
