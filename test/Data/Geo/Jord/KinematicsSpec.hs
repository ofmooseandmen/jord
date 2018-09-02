module Data.Geo.Jord.KinematicsSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Maybe (fromJust)
import Test.Hspec

spec :: Spec
spec =
    describe "kinematics" $ do
        describe "position" $ do
            it "computes position at t from p0, bearing and speed" $ do
                let p0 = latLongHeight (readLatLong "531914N0014347W") (metres 15000)
                let p1 = decimalLatLongHeight 53.1882691 0.1332741 (metres 15000)
                let t = Track p0 (decimalDegrees 96.0217) (kilometresPerHour 124.8)
                position84 t (hours 1) `shouldBe` p1
            it "handles poles" $ do
                -- distance between poles assuming a spherical earth (WGS84) = 20015.114352200002km
                -- track at north pole travelling at 20015.114352200002km/h and true north reaches the
                -- south pole after 1 hour.
                let t = Track (decimalLatLong 90 0) zero (kilometresPerHour 20015.114352200002)
                position84 t (hours 1) `shouldBe` decimalLatLong (-90) 180.0
            it "return p0 if speed is 0" $ do
                let p0 = latLongHeight (readLatLong "531914N0014347W") (metres 15000)
                let t = Track p0 (decimalDegrees 96.0217) zero
                position84 t (hours 1) `shouldBe` p0
            it "return p0 if duration is 0" $ do
                let p0 = latLongHeight (readLatLong "531914N0014347W") (metres 15000)
                let t = Track p0 (decimalDegrees 96.0217) (kilometresPerHour 124.8)
                position84 t zero `shouldBe` p0
        describe "cpa" $ do
            it "handles trailing tracks" $ do
                let p1 = decimalLatLong 20 30
                let px = destination84 p1 (decimalDegrees 20) (kilometres 1)
                let p2 = interpolate p1 px 0.25
                let b1 = fromJust (initialBearing p1 px)
                let b2 = fromJust (initialBearing p2 px)
                let t1 = Track p1 b1 (knots 400)
                let t2 = Track p2 b2 (knots 400)
                let c = cpa84 t1 t2
                -- any time is correct but it should be close to zero since that's
                -- our initial value
                fmap (\r -> toMilliseconds (cpaTime r) < 5000) c `shouldBe` Just True
                fmap cpaDistance c `shouldBe` Just (metres 250.0036)
            it "handles heading tracks" $ do
                let p1 = decimalLatLong 20 30
                let p2 = decimalLatLong 21 31
                let b1 = fromJust (initialBearing p1 p2)
                let b2 = fromJust (initialBearing p2 p1)
                let t1 = Track p1 b1 (knots 400)
                let t2 = Track p2 b2 (knots 400)
                let c = cpa84 t1 t2
                -- distance between p1 and p2 = 152.354309 km
                -- speed = 740.8 km/h
                -- time = 152.354309 / 740.8 / 2
                fmap cpaTime c `shouldBe` Just (milliseconds 370191)
                fmap cpaDistance c `shouldBe` Just zero
            it "handles tracks at the same position" $ do
                let p = decimalLatLong 20 30
                let t1 = Track p (decimalDegrees 45) (knots 300)
                let t2 = Track p (decimalDegrees 135) (knots 500)
                let c = cpa84 t1 t2
                fmap cpaTime c `shouldBe` Just zero
                fmap cpaDistance c `shouldBe` Just zero
            it "computes time to CPA, positions and distance at CPA" $ do
                let p1 = decimalLatLong 20 (-60)
                let b1 = decimalDegrees 10
                let s1 = knots 15
                let p2 = decimalLatLong 34 (-50)
                let b2 = decimalDegrees 220
                let s2 = knots 300
                let t1 = Track p1 b1 s1
                let t2 = Track p2 b2 s2
                let c = cpa84 t1 t2
                fmap cpaTime c `shouldBe` Just (milliseconds 11396155)
                fmap cpaDistance c `shouldBe` Just (kilometres 124.2317453)
            it "returns Nothing if time to CPA is in the past" $ do
                let t1 = Track (decimalLatLong 30 30) (decimalDegrees 45) (knots 400)
                let t2 = Track (decimalLatLong 30.01 30) (decimalDegrees 315) (knots 400)
                cpa84 t1 t2 `shouldBe` Nothing
        describe "intercept" $ do
            it "returns Nothing if target and interceptor are at the same position" $
                intercept84
                    (Track (decimalLatLong 30 30) (decimalDegrees 45) (knots 400))
                    (decimalLatLong 30 30) `shouldBe`
                Nothing
            it "returns Nothing if interceptor is on the great circle of target and behind" $ do
                -- minimum speed would be ideally target speed + epsillon.
                let ip = decimalLatLong 20 30
                let px = destination84 ip (decimalDegrees 20) (kilometres 1)
                let tp = interpolate ip px 0.25
                let b = fromJust (initialBearing tp px)
                let t = Track tp b (knots 400)
                intercept84 t ip `shouldBe` Nothing
            it "handles interceptor on the great circle of target and in front" $ do
                let tp = decimalLatLong 20 30
                let px = destination84 tp (decimalDegrees 20) (kilometres 1)
                let ip = interpolate tp px 0.25
                let b = fromJust (initialBearing tp px)
                let t = Track tp b (knots 400)
                let i = intercept84 t ip
                fmap interceptorSpeed i `shouldBe` Just zero
                fmap interceptPosition i `shouldBe` Just ip
                fmap interceptTime i `shouldBe` Just (seconds 1.215)
            it "returns the minimum speed required for intercept to take place" $ do
                let t = Track (decimalLatLong 34 (-50)) (decimalDegrees 220) (knots 600)
                let ip = decimalLatLong 20 (-60)
                let i = intercept84 t ip
                fmap interceptorSpeed i `shouldBe` Just (knots 52.633367756059)
                fmap interceptTime i `shouldBe` Just (seconds 5993.831)
                let interceptor =
                        Track
                            ip
                            (fromJust (fmap interceptorBearing i))
                            (fromJust (fmap interceptorSpeed i))
                fmap interceptPosition i `shouldBe`
                    Just (position84 interceptor (fromJust (fmap interceptTime i)))
        describe "interceptBySpeed" $ do
            it "returns Nothing if target and interceptor are at the same position" $
                interceptBySpeed84
                    (Track (decimalLatLong 30 30) (decimalDegrees 45) (knots 400))
                    (decimalLatLong 30 30)
                    (knots 400) `shouldBe`
                Nothing
            it "returns Nothing if interceptor speed is below minimum speed" $ do
                let t = Track (decimalLatLong 34 (-50)) (decimalDegrees 220) (knots 600)
                let ip = decimalLatLong 20 (-60)
                interceptBySpeed84 t ip (knots 50) `shouldBe` Nothing
            it "handles interceptor on the great circle of target and behind" $ do
                let ip = decimalLatLong 20 30
                let px = destination84 ip (decimalDegrees 20) (kilometres 1)
                let tp = interpolate ip px 0.25
                let b = fromJust (initialBearing tp px)
                let t = Track tp b (metresPerSecond 400)
                let i = interceptBySpeed84 t ip (metresPerSecond 500)
                fmap interceptTime i `shouldBe` Just (seconds 2.5)
            it "returns the speed needed for intercept to take place" $ do
                let t = Track (decimalLatLong 34 (-50)) (decimalDegrees 220) (knots 600)
                let ip = decimalLatLong 20 (-60)
                let i = interceptBySpeed84 t ip (knots 700)
                fmap interceptTime i `shouldBe` Just (seconds 2764.692)
                fmap interceptorBearing i `shouldBe` Just (decimalDegrees 25.93541277)
                fmap interceptDistance i `shouldBe` Just (kilometres 995.5960805999999)
            it "returns the same as intercept when called with minimum speed" $ do
                let t = Track (decimalLatLong 45 50) (decimalDegrees 54) (knots 500)
                let ip = decimalLatLong 70 30
                let mi = intercept84 t ip
                let i = interceptBySpeed84 t ip (fromJust (fmap interceptorSpeed mi))
                fmap interceptTime i `shouldBe` fmap interceptTime mi
        describe "interceptByTime" $ do
            it "returns Nothing if duration is zero" $
                interceptByTime84
                    (Track (decimalLatLong 30 30) (decimalDegrees 45) (knots 400))
                    (decimalLatLong 34 (-50))
                    zero `shouldBe`
                Nothing
            it "returns Nothing if duration is negative" $
                interceptByTime84
                    (Track (decimalLatLong 30 30) (decimalDegrees 45) (knots 400))
                    (decimalLatLong 34 (-50))
                    (seconds (-1)) `shouldBe`
                Nothing
            it "returns Nothing if target and interceptor are at the same position" $
                interceptByTime84
                    (Track (decimalLatLong 30 30) (decimalDegrees 45) (knots 400))
                    (decimalLatLong 30 30)
                    (seconds 10) `shouldBe`
                Nothing
            it "returns the speed needed for intercept to take place" $ do
                let t = Track (decimalLatLong 34 (-50)) (decimalDegrees 220) (knots 600)
                let ip = decimalLatLong 20 (-60)
                let d = seconds 2700
                let i = interceptByTime84 t ip d
                fmap interceptorSpeed i `shouldBe` Just (knots 730.959238)
                fmap interceptorBearing i `shouldBe` Just (decimalDegrees 26.1199030)
                fmap interceptPosition i `shouldBe` Just (decimalLatLong 28.1366797 (-55.4559475))
                fmap interceptDistance i `shouldBe` Just (metres 1015302.3815)
                fmap interceptTime i `shouldBe` Just (seconds 2700)
            it "handles the poles" $ do
                -- distance between poles assuming a spherical earth (WGS84) = 20015.114352200002km
                -- target at north pole travelling at 500km/h and true north can be intercepted from
                -- the south pole by an interceptor travelling at ~ 19515.114352200002km/h and 180 degrees.
                let t = Track (decimalLatLong 90 0) zero (kilometresPerHour 500)
                let ip = decimalLatLong (-90) 0
                let i = interceptByTime84 t ip (seconds 3600)
                fmap interceptorSpeed i `shouldBe` Just (kilometresPerHour 19515.11434)
                fmap interceptorBearing i `shouldBe` Just (decimalDegrees 180)
            it "handles the interceptor being at the intercept position at t" $ do
                let tp = decimalLatLong 34 (-50)
                let t = Track tp (decimalDegrees 220) (knots 600)
                let d = seconds 3600
                let ip = position84 t d
                let i = interceptByTime84 t ip d
                fmap interceptorSpeed i `shouldBe` Just zero
                fmap interceptorBearing i `shouldBe` initialBearing ip tp
