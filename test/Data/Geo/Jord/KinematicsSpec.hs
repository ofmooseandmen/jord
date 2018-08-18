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
                let b = decimalDegrees 96.0217
                let s = kilometresPerHour 124.8
                let p1 = decimalLatLongHeight 53.1882691 0.1332741 (metres 15000)
                let t = Track p0 b s
                position84 t (hours 1) `shouldBe` p1
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
