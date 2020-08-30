module Data.Geo.Jord.SpeedSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Speed as Speed

spec :: Spec
spec = do
    describe "Reading valid speeds" $ do
        it "reads -15.2m/s" $ Speed.read "-15.2m/s" `shouldBe` Just (Speed.metresPerSecond (-15.2))
        it "reads 154km/h" $ Speed.read "154km/h" `shouldBe` Just (Speed.kilometresPerHour 154)
        it "reads 200mph" $ Speed.read "200mph" `shouldBe` Just (Speed.milesPerHour 200)
        it "reads 400kt" $ Speed.read "400kt" `shouldBe` Just (Speed.knots 400)
        it "reads 1ft/s" $ Speed.read "1ft/s" `shouldBe` Just (Speed.feetPerSecond 1)
    describe "Reading invalid speeds" $ do
        it "fails to read 5" $ Speed.read "5" `shouldBe` Nothing
        it "fails to read 5mps" $ Speed.read "5mps" `shouldBe` Nothing
    describe "Showing speeds" $
        it "shows speed in kilometres per hour" $
        show (Speed.kilometresPerHour 154) `shouldBe` "154.0km/h"
    describe "Converting speeds" $ do
        it "converts metres per seconds to kilometres per hour" $
            Speed.toKilometresPerHour (Speed.metresPerSecond 100) `shouldBe` 360.0
        it "converts metres per seconds to miles per hour" $
            Speed.toMilesPerHour (Speed.metresPerSecond 100) `shouldBe` 223.69362920544023
        it "converts kilometres per hour to Speed.knots" $
            Speed.toKnots (Speed.kilometresPerHour 1000) `shouldBe` 539.9568034557235
        it "converts feet per second to kilometres per hour" $
            Speed.toKilometresPerHour (Speed.feetPerSecond 1) `shouldBe` 1.09728
    describe "Resolution" $ do
        it "handles 1 km/h" $ Speed.toKilometresPerHour (Speed.kilometresPerHour 1) `shouldBe` 1
        it "handles 1 m/s" $ Speed.toMetresPerSecond (Speed.metresPerSecond 1) `shouldBe` 1
        it "handles 1 mph" $ Speed.toMilesPerHour (Speed.milesPerHour 1) `shouldBe` 1
        it "handles 1 knot" $ Speed.toKnots (Speed.knots 1) `shouldBe` 1
        it "handles 1 fp/s" $ Speed.toFeetPerSecond (Speed.feetPerSecond 1) `shouldBe` 1
    describe "Adding/Subtracting speeds" $ do
        it "adds speeds" $
            Speed.add (Speed.kilometresPerHour 1000) (Speed.metresPerSecond 1000) `shouldBe`
            Speed.kilometresPerHour 4600
        it "subtracts speeds" $
            Speed.subtract (Speed.metresPerSecond 1000) (Speed.knots 10.5) `shouldBe`
            Speed.kilometresPerHour 3580.554
