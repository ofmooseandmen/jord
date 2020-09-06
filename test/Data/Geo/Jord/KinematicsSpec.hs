module Data.Geo.Jord.KinematicsSpec
    ( spec
    ) where

import Data.Maybe (fromJust)

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Duration as Duration
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import Data.Geo.Jord.Kinematics (Track(..))
import qualified Data.Geo.Jord.Kinematics as Kinematics
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Models (S84(..))
import qualified Data.Geo.Jord.Speed as Speed

spec :: Spec
spec =
    describe "kinematics" $ do
        describe "trackPositionAfter" $ do
            it "computes position at t from p0, bearing and speed" $ do
                let p0 = Geodetic.s84Pos 53.320556 (-1.729722)
                let p1 = Geodetic.s84Pos 53.18826954833333 0.13327449083333334
                let t = Track p0 (Angle.decimalDegrees 96.0217) (Speed.kilometresPerHour 124.8)
                Kinematics.trackPositionAfter t (Duration.hours 1) `shouldBe` p1
            it "handles crossing the date line" $
                -- distance at equator between 2 positions separated by 180 degrees assuming a spherical
                -- earth (from WGS84) = 20015.114352233km
                -- track at 0N1E travelling at 20015.114352233km/h and 90 degrees reaches 0N179W after 1 hour.
             do
                let p0 = Geodetic.s84Pos 0 1
                let t = Track p0 (Angle.decimalDegrees 90) (Speed.kilometresPerHour 20015.114352233)
                let p1 = Kinematics.trackPositionAfter t (Duration.hours 1)
                GreatCircle.distance p1 (Geodetic.s84Pos 0 (-179)) <
                    Length.metres 0.001 `shouldBe` True
            it "handles poles" $
                -- distance between poles assuming a spherical earth (from WGS84) = 20015.114352233km
                -- track at north pole travelling at 20015.114352233km/h and true north reaches the
                -- south pole after 1 hour.
             do
                let t =
                        Track
                            (Geodetic.northPole S84)
                            Angle.zero
                            (Speed.kilometresPerHour 20015.114352233)
                let p1 = Kinematics.trackPositionAfter t (Duration.hours 1)
                GreatCircle.distance p1 (Geodetic.southPole S84) <
                    Length.metres 0.001 `shouldBe` True
            it "return p0 if speed is 0" $ do
                let p0 = Geodetic.s84Pos 53.320556 (-1.729722)
                let t = Track p0 (Angle.decimalDegrees 96.0217) Speed.zero
                Kinematics.trackPositionAfter t (Duration.hours 1) `shouldBe` p0
            it "return p0 if duration is 0" $ do
                let p0 = Geodetic.s84Pos 53.320556 (-1.729722)
                let t = Track p0 (Angle.decimalDegrees 96.0217) (Speed.kilometresPerHour 124.8)
                Kinematics.trackPositionAfter t Duration.zero `shouldBe` p0
        describe "cpa" $ do
            it "returns nothing for trailing tracks at same speed" $ do
                let p1 = Geodetic.s84Pos 20 30
                let px = GreatCircle.destination p1 (Angle.decimalDegrees 20) (Length.kilometres 1)
                let p2 = GreatCircle.interpolated p1 px 0.25
                let b1 = fromJust (GreatCircle.initialBearing p1 px)
                let b2 = fromJust (GreatCircle.initialBearing p2 px)
                let ownship = Track p1 b1 (Speed.knots 400)
                let intruder = Track p2 b2 (Speed.knots 400)
                Kinematics.cpa ownship intruder `shouldBe` Nothing
            it "returns nothing for trailing tracks with track ahead escaping" $ do
                let p1 = Geodetic.s84Pos 20 30
                let px = GreatCircle.destination p1 (Angle.decimalDegrees 20) (Length.kilometres 1)
                let p2 = GreatCircle.interpolated p1 px 0.25
                let b1 = fromJust (GreatCircle.initialBearing p1 px)
                let b2 = fromJust (GreatCircle.initialBearing p2 px)
                let ownship = Track p1 b1 (Speed.knots 400)
                let intruder = Track p2 b2 (Speed.knots 401)
                Kinematics.cpa ownship intruder `shouldBe` Nothing
            it "handles trailing tracks with track behind catching up" $ do
                let p1 = Geodetic.s84Pos 20 30
                let px = GreatCircle.destination p1 (Angle.decimalDegrees 20) (Length.kilometres 1)
                let p2 = GreatCircle.interpolated p1 px 0.25
                let b1 = fromJust (GreatCircle.initialBearing p1 px)
                let b2 = fromJust (GreatCircle.initialBearing p2 px)
                let ownship = Track p1 b1 (Speed.knots 402)
                let intruder = Track p2 b2 (Speed.knots 400)
                let cpa = Kinematics.cpa ownship intruder
                fmap Kinematics.timeToCpa cpa `shouldBe` Just (Duration.seconds 242.981)
                fmap Kinematics.distanceAtCpa cpa `shouldBe` Just (Length.metres 5.25e-4) -- close to 0
            it "handles heading tracks" $ do
                let p1 = Geodetic.s84Pos 20 30
                let p2 = Geodetic.s84Pos 21 31
                let b1 = fromJust (GreatCircle.initialBearing p1 p2)
                let b2 = fromJust (GreatCircle.initialBearing p2 p1)
                let ownship = Track p1 b1 (Speed.knots 400)
                let intruder = Track p2 b2 (Speed.knots 400)
                let cpa = Kinematics.cpa ownship intruder
                -- distance between p1 and p2 = 152.354309 km
                -- speed = 740.8 km/h
                -- time = 152.354309 / 740.8 / 2
                fmap Kinematics.timeToCpa cpa `shouldBe` Just (Duration.milliseconds 370191)
                fmap Kinematics.distanceAtCpa cpa `shouldBe` Just Length.zero
            it "handles tracks at the same position" $ do
                let p = Geodetic.s84Pos 20 30
                let ownship = Track p (Angle.decimalDegrees 45) (Speed.knots 300)
                let intruder = Track p (Angle.decimalDegrees 135) (Speed.knots 500)
                let cpa = Kinematics.cpa ownship intruder
                fmap Kinematics.timeToCpa cpa `shouldBe` Just Duration.zero
                fmap Kinematics.distanceAtCpa cpa `shouldBe` Just Length.zero
            it "computes time to CPA, positions and distance at CPA" $ do
                let p1 = Geodetic.s84Pos 20 (-60)
                let b1 = Angle.decimalDegrees 10
                let s1 = Speed.knots 15
                let p2 = Geodetic.s84Pos 34 (-50)
                let b2 = Angle.decimalDegrees 220
                let s2 = Speed.knots 300
                let ownship = Track p1 b1 s1
                let intruder = Track p2 b2 s2
                let cpa = Kinematics.cpa ownship intruder
                fmap Kinematics.timeToCpa cpa `shouldBe` Just (Duration.milliseconds 11396155)
                fmap Kinematics.distanceAtCpa cpa `shouldBe` Just (Length.kilometres 124.231730834)
                fmap Kinematics.cpaOwnshipPosition cpa `shouldBe`
                    Just (Geodetic.s84Pos 20.778789303333333 (-59.85311827861111))
                fmap Kinematics.cpaIntruderPosition cpa `shouldBe`
                    Just (Geodetic.s84Pos 21.402367759166665 (-60.846710862222224))
            it "returns Nothing if time to CPA is in the past" $ do
                let ownship =
                        Track (Geodetic.s84Pos 30 30) (Angle.decimalDegrees 45) (Speed.knots 400)
                let intruder =
                        Track
                            (Geodetic.s84Pos 30.01 30)
                            (Angle.decimalDegrees 315)
                            (Speed.knots 400)
                Kinematics.cpa ownship intruder `shouldBe` Nothing
        describe "intercept" $ do
            it "returns Nothing if target and interceptor are at the same position" $
                Kinematics.intercept
                    (Track (Geodetic.s84Pos 30 30) (Angle.decimalDegrees 45) (Speed.knots 400))
                    (Geodetic.s84Pos 30 30) `shouldBe`
                Nothing
            it "returns Nothing if interceptor is behind target" $ do
                let t = Track (Geodetic.s84Pos 45 67) (Angle.decimalDegrees 54) (Speed.knots 400)
                let ip = Geodetic.s84Pos 44 66
                Kinematics.intercept t ip `shouldBe` Nothing
            it "handles interceptor on the great circle of target and in front" $ do
                let tp = Geodetic.s84Pos 20 30
                let b = Angle.decimalDegrees 12
                let t = Track tp b (Speed.knots 400)
                let ip = Kinematics.trackPositionAfter t (Duration.minutes 1)
                let i = Kinematics.intercept t ip
                fmap Kinematics.interceptorSpeed i `shouldBe` Just Speed.zero
                fmap Kinematics.distanceToIntercept i `shouldBe` Just Length.zero
                fmap Kinematics.interceptPosition i `shouldBe` Just ip
                fmap Kinematics.timeToIntercept i `shouldBe` Just (Duration.minutes 1)
            it "returns the minimum speed required for intercept to take place" $ do
                let t =
                        Track
                            (Geodetic.s84Pos 34 (-50))
                            (Angle.decimalDegrees 220)
                            (Speed.knots 600)
                let ip = Geodetic.s84Pos 20 (-60)
                let i = Kinematics.intercept t ip
                fmap Kinematics.interceptorSpeed i `shouldBe` Just (Speed.knots 52.63336879049676)
                fmap Kinematics.timeToIntercept i `shouldBe` Just (Duration.seconds 5993.831)
                let interceptor =
                        Track
                            ip
                            (Kinematics.interceptorBearing (fromJust i))
                            (Kinematics.interceptorSpeed (fromJust i))
                let ep =
                        Kinematics.trackPositionAfter
                            interceptor
                            (Kinematics.timeToIntercept (fromJust i))
                let ap = fmap Kinematics.interceptPosition i
                let d = fmap (GreatCircle.distance ep) ap
                fmap (<= Length.metres 0.001) d `shouldBe` Just True
        describe "interceptBySpeed" $ do
            it "returns Nothing if target and interceptor are at the same position" $
                Kinematics.interceptBySpeed
                    (Track (Geodetic.s84Pos 30 30) (Angle.decimalDegrees 45) (Speed.knots 400))
                    (Geodetic.s84Pos 30 30)
                    (Speed.knots 400) `shouldBe`
                Nothing
            it "returns Nothing if interceptor speed is below minimum speed" $ do
                let t =
                        Track
                            (Geodetic.s84Pos 34 (-50))
                            (Angle.decimalDegrees 220)
                            (Speed.knots 600)
                let ip = Geodetic.s84Pos 20 (-60)
                Kinematics.interceptBySpeed t ip (Speed.knots 50) `shouldBe` Nothing
            it "returns the time needed for intercept to take place" $ do
                let t =
                        Track
                            (Geodetic.s84Pos 34 (-50))
                            (Angle.decimalDegrees 220)
                            (Speed.knots 600)
                let ip = Geodetic.s84Pos 20 (-60)
                let i = Kinematics.interceptBySpeed t ip (Speed.knots 700)
                fmap Kinematics.timeToIntercept i `shouldBe` Just (Duration.seconds 2764.692)
                fmap Kinematics.interceptorBearing i `shouldBe`
                    Just (Angle.decimalDegrees 25.935412485277777)
                fmap Kinematics.distanceToIntercept i `shouldBe`
                    Just (Length.kilometres 995.596069189)
            it "returns the same as intercept when called with minimum speed" $ do
                let t = Track (Geodetic.s84Pos 45 50) (Angle.decimalDegrees 54) (Speed.knots 500)
                let ip = Geodetic.s84Pos 70 30
                let mi = Kinematics.intercept t ip
                let i = Kinematics.interceptBySpeed t ip (Kinematics.interceptorSpeed (fromJust mi))
                fmap Kinematics.timeToIntercept i `shouldBe` fmap Kinematics.timeToIntercept mi
        describe "interceptByTime" $ do
            it "returns Nothing if duration is 0" $
                Kinematics.interceptByTime
                    (Track (Geodetic.s84Pos 30 30) (Angle.decimalDegrees 45) (Speed.knots 400))
                    (Geodetic.s84Pos 34 (-50))
                    Duration.zero `shouldBe`
                Nothing
            it "returns Nothing if duration is negative" $
                Kinematics.interceptByTime
                    (Track (Geodetic.s84Pos 30 30) (Angle.decimalDegrees 45) (Speed.knots 400))
                    (Geodetic.s84Pos 34 (-50))
                    (Duration.seconds (-1)) `shouldBe`
                Nothing
            it "returns Nothing if target and interceptor are at the same position" $
                Kinematics.interceptByTime
                    (Track (Geodetic.s84Pos 30 30) (Angle.decimalDegrees 45) (Speed.knots 400))
                    (Geodetic.s84Pos 30 30)
                    (Duration.seconds 10) `shouldBe`
                Nothing
            it "returns the speed needed for intercept to take place" $ do
                let t =
                        Track
                            (Geodetic.s84Pos 34 (-50))
                            (Angle.decimalDegrees 220)
                            (Speed.knots 600)
                let ip = Geodetic.s84Pos 20 (-60)
                let d = Duration.seconds 2700
                let i = Kinematics.interceptByTime t ip d
                fmap Kinematics.interceptorSpeed i `shouldBe` Just (Speed.knots 730.9592213822895)
                fmap Kinematics.interceptorBearing i `shouldBe`
                    Just (Angle.decimalDegrees 26.11990256388889)
                fmap Kinematics.interceptPosition i `shouldBe`
                    Just (Geodetic.s84Pos 28.136679674444444 (-55.455947612222225))
                fmap Kinematics.distanceToIntercept i `shouldBe`
                    Just (Length.kilometres 1015.302358852)
                fmap Kinematics.timeToIntercept i `shouldBe` Just (Duration.seconds 2700)
            it "handles the poles" $
                -- distance between poles assuming a spherical earth (WGS84) = 20015.114352200002km
                -- target at north pole travelling at 500km/h and true north can be intercepted from
                -- the south pole by an interceptor travelling at ~ 19515.114352200002km/h and 180 degrees.
             do
                let t = Track (Geodetic.northPole S84) Angle.zero (Speed.kilometresPerHour 500)
                let ip = Geodetic.southPole S84
                let i = Kinematics.interceptByTime t ip (Duration.seconds 3600)
                fmap Kinematics.interceptorSpeed i `shouldBe`
                    Just (Speed.kilometresPerHour 19515.114352)
                fmap Kinematics.interceptorBearing i `shouldBe` Just (Angle.decimalDegrees 180)
            it "handles the interceptor being at the intercept position at t" $ do
                let tp = Geodetic.s84Pos 34 (-50)
                let t = Track tp (Angle.decimalDegrees 220) (Speed.knots 600)
                let d = Duration.seconds 3600
                let ip = Kinematics.trackPositionAfter t d
                let i = Kinematics.interceptByTime t ip d
                fmap Kinematics.interceptorSpeed i `shouldBe` Just Speed.zero
                fmap Kinematics.interceptorBearing i `shouldBe` GreatCircle.initialBearing ip tp
