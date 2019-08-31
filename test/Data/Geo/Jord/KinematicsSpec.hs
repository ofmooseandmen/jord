module Data.Geo.Jord.KinematicsSpec
    ( spec
    ) where

import Data.Maybe (fromJust)

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec =
    describe "kinematics" $ do
        describe "trackPositionAfter" $ do
            it "computes position at t from p0, bearing and speed" $ do
                let p0 = s84Pos 53.320556 (-1.729722) (metres 15000)
                let p1 = s84Pos 53.1882697 0.1332744 (metres 15000)
                let t = Track p0 (decimalDegrees 96.0217) (kilometresPerHour 124.8)
                trackPositionAfter t (hours 1) `shouldBe` p1
            it "handles crossing the date line" $
                -- distance at equator between 2 positions separated by 180 degrees assuming a spherical
                -- earth (from WGS84) = 20015.114352200002km
                -- track at 0N1E travelling at 20015.114352200002km/h and 90 degrees reaches the
                -- 0N179W pole after 1 hour.
             do
                let p0 = s84Pos 0 1 zero
                let t = Track p0 (decimalDegrees 90) (kilometresPerHour 20015.114352200002)
                trackPositionAfter t (hours 1) `shouldBe` s84Pos 0 (-179) zero
            it "handles poles" $
                -- distance between poles assuming a spherical earth (from WGS84) = 20015.114352200002km
                -- track at north pole travelling at 20015.114352200002km/h and true north reaches the
                -- south pole after 1 hour.
             do
                let t = Track (northPole S84) zero (kilometresPerHour 20015.114352200002)
                latitude (trackPositionAfter t (hours 1)) `shouldBe` decimalDegrees (-90)
            it "return p0 if speed is 0" $ do
                let p0 = s84Pos 53.320556 (-1.729722) (metres 15000)
                let t = Track p0 (decimalDegrees 96.0217) zero
                trackPositionAfter t (hours 1) `shouldBe` p0
            it "return p0 if duration is 0" $ do
                let p0 = s84Pos 53.320556 (-1.729722) (metres 15000)
                let t = Track p0 (decimalDegrees 96.0217) (kilometresPerHour 124.8)
                trackPositionAfter t zero `shouldBe` p0
        describe "cpa" $ do
            it "returns nothing for trailing tracks at same speed" $ do
                let p1 = s84Pos 20 30 zero
                let px = destination p1 (decimalDegrees 20) (kilometres 1)
                let p2 = interpolate p1 px 0.25
                let b1 = fromJust (initialBearing p1 px)
                let b2 = fromJust (initialBearing p2 px)
                let t1 = Track p1 b1 (knots 400)
                let t2 = Track p2 b2 (knots 400)
                cpa t1 t2 `shouldBe` Nothing
            it "returns nothing for trailing tracks with track ahead escaping" $ do
                let p1 = s84Pos 20 30 zero
                let px = destination p1 (decimalDegrees 20) (kilometres 1)
                let p2 = interpolate p1 px 0.25
                let b1 = fromJust (initialBearing p1 px)
                let b2 = fromJust (initialBearing p2 px)
                let t1 = Track p1 b1 (knots 400)
                let t2 = Track p2 b2 (knots 401)
                cpa t1 t2 `shouldBe` Nothing
            it "handles trailing tracks with track behind catching up" $ do
                let p1 = s84Pos 20 30 zero
                let px = destination p1 (decimalDegrees 20) (kilometres 1)
                let p2 = interpolate p1 px 0.25
                let b1 = fromJust (initialBearing p1 px)
                let b2 = fromJust (initialBearing p2 px)
                let t1 = Track p1 b1 (knots 401)
                let t2 = Track p2 b2 (knots 400)
                let c = cpa t1 t2
                fmap cpaTime c `shouldBe` Just (seconds 483.042)
                fmap cpaDistance c `shouldBe` Just (metres 1.5135)
            it "handles heading tracks" $ do
                let p1 = s84Pos 20 30 zero
                let p2 = s84Pos 21 31 zero
                let b1 = fromJust (initialBearing p1 p2)
                let b2 = fromJust (initialBearing p2 p1)
                let t1 = Track p1 b1 (knots 400)
                let t2 = Track p2 b2 (knots 400)
                let c = cpa t1 t2
                -- distance between p1 and p2 = 152.354309 km
                -- speed = 740.8 km/h
                -- time = 152.354309 / 740.8 / 2
                fmap cpaTime c `shouldBe` Just (milliseconds 370191)
                fmap cpaDistance c `shouldBe` Just zero
            it "handles tracks at the same position" $ do
                let p = s84Pos 20 30 zero
                let t1 = Track p (decimalDegrees 45) (knots 300)
                let t2 = Track p (decimalDegrees 135) (knots 500)
                let c = cpa t1 t2
                fmap cpaTime c `shouldBe` Just zero
                fmap cpaDistance c `shouldBe` Just zero
            it "computes time to CPA, positions and distance at CPA" $ do
                let p1 = s84Pos 20 (-60) zero
                let b1 = decimalDegrees 10
                let s1 = knots 15
                let p2 = s84Pos 34 (-50) zero
                let b2 = decimalDegrees 220
                let s2 = knots 300
                let t1 = Track p1 b1 s1
                let t2 = Track p2 b2 s2
                let c = cpa t1 t2
                fmap cpaTime c `shouldBe` Just (milliseconds 11396155)
                fmap cpaDistance c `shouldBe` Just (kilometres 124.2317453)
            it "returns Nothing if time to CPA is in the past" $ do
                let t1 = Track (s84Pos 30 30 zero) (decimalDegrees 45) (knots 400)
                let t2 = Track (s84Pos 30.01 30 zero) (decimalDegrees 315) (knots 400)
                cpa t1 t2 `shouldBe` Nothing
        describe "intercept" $ do
            it "returns Nothing if target and interceptor are at the same position" $
                intercept
                    (Track (s84Pos 30 30 zero) (decimalDegrees 45) (knots 400))
                    (s84Pos 30 30 zero) `shouldBe`
                Nothing
            it "returns Nothing if interceptor is behind target" $ do
                let t = Track (s84Pos 45 67 zero) (decimalDegrees 54) (knots 400)
                let ip = s84Pos 44 66 zero
                intercept t ip `shouldBe` Nothing
            it "handles interceptor on the great circle of target and in front" $ do
                let tp = s84Pos 20 30 zero
                let b = decimalDegrees 12
                let t = Track tp b (knots 400)
                let ip = trackPositionAfter t (minutes 1)
                let i = intercept t ip
                fmap interceptorSpeed i `shouldBe` Just zero
                fmap interceptDistance i `shouldBe` Just zero
                fmap interceptPosition i `shouldBe` Just ip
                fmap interceptTime i `shouldBe` Just (minutes 1)
            it "returns the minimum speed required for intercept to take place" $ do
                let t = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 600)
                let ip = s84Pos 20 (-60) zero
                let i = intercept t ip
                fmap interceptorSpeed i `shouldBe` Just (knots 52.633367756059)
                fmap interceptTime i `shouldBe` Just (seconds 5993.831)
                let interceptor =
                        Track
                            ip
                            (fromJust (fmap interceptorBearing i))
                            (fromJust (fmap interceptorSpeed i))
                fmap interceptPosition i `shouldBe`
                    Just (trackPositionAfter interceptor (fromJust (fmap interceptTime i)))
        describe "interceptBySpeed" $ do
            it "returns Nothing if target and interceptor are at the same position" $
                interceptBySpeed
                    (Track (s84Pos 30 30 zero) (decimalDegrees 45) (knots 400))
                    (s84Pos 30 30 zero)
                    (knots 400) `shouldBe`
                Nothing
            it "returns Nothing if interceptor speed is below minimum speed" $ do
                let t = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 600)
                let ip = s84Pos 20 (-60) zero
                interceptBySpeed t ip (knots 50) `shouldBe` Nothing
            it "returns the speed needed for intercept to take place" $ do
                let t = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 600)
                let ip = s84Pos 20 (-60) zero
                let i = interceptBySpeed t ip (knots 700)
                fmap interceptTime i `shouldBe` Just (seconds 2764.692)
                fmap interceptorBearing i `shouldBe` Just (decimalDegrees 25.9354125)
                fmap interceptDistance i `shouldBe` Just (kilometres 995.5960805999999)
            it "returns the same as intercept when called with minimum speed" $ do
                let t = Track (s84Pos 45 50 zero) (decimalDegrees 54) (knots 500)
                let ip = s84Pos 70 30 zero
                let mi = intercept t ip
                let i = interceptBySpeed t ip (fromJust (fmap interceptorSpeed mi))
                fmap interceptTime i `shouldBe` fmap interceptTime mi
        describe "interceptByTime" $ do
            it "returns Nothing if duration is zero" $
                interceptByTime
                    (Track (s84Pos 30 30 zero) (decimalDegrees 45) (knots 400))
                    (s84Pos 34 (-50) zero)
                    zero `shouldBe`
                Nothing
            it "returns Nothing if duration is negative" $
                interceptByTime
                    (Track (s84Pos 30 30 zero) (decimalDegrees 45) (knots 400))
                    (s84Pos 34 (-50) zero)
                    (seconds (-1)) `shouldBe`
                Nothing
            it "returns Nothing if target and interceptor are at the same position" $
                interceptByTime
                    (Track (s84Pos 30 30 zero) (decimalDegrees 45) (knots 400))
                    (s84Pos 30 30 zero)
                    (seconds 10) `shouldBe`
                Nothing
            it "returns the speed needed for intercept to take place" $ do
                let t = Track (s84Pos 34 (-50) zero) (decimalDegrees 220) (knots 600)
                let ip = s84Pos 20 (-60) zero
                let d = seconds 2700
                let i = interceptByTime t ip d
                fmap interceptorSpeed i `shouldBe` Just (knots 730.9592154427646)
                fmap interceptorBearing i `shouldBe` Just (decimalDegrees 26.1199025)
                fmap interceptPosition i `shouldBe` Just (s84Pos 28.1366797 (-55.4559475) zero)
                fmap interceptDistance i `shouldBe` Just (kilometres 1015.3023506)
                fmap interceptTime i `shouldBe` Just (seconds 2700)
            it "handles the poles" $
                -- distance between poles assuming a spherical earth (WGS84) = 20015.114352200002km
                -- target at north pole travelling at 500km/h and true north can be intercepted from
                -- the south pole by an interceptor travelling at ~ 19515.114352200002km/h and 0 degrees.
             do
                let t = Track (northPole S84) zero (kilometresPerHour 500)
                let ip = southPole S84
                let i = interceptByTime t ip (seconds 3600)
                fmap interceptorSpeed i `shouldBe` Just (kilometresPerHour 19515.11434)
                fmap interceptorBearing i `shouldBe` Just (decimalDegrees 0)
            it "handles the interceptor being at the intercept position at t" $ do
                let tp = s84Pos 34 (-50) zero
                let t = Track tp (decimalDegrees 220) (knots 600)
                let d = seconds 3600
                let ip = trackPositionAfter t d
                let i = interceptByTime t ip d
                fmap interceptorSpeed i `shouldBe` Just zero
                fmap interceptorBearing i `shouldBe` initialBearing ip tp