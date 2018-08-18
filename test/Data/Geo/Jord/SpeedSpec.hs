module Data.Geo.Jord.SpeedSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid speeds" $ do
        it "reads -15.2m/s" $ readSpeed "-15.2m/s" `shouldBe` metresPerSecond (-15.2)
        it "reads 154km/h" $ readSpeed "154km/h" `shouldBe` kilometresPerHour 154
        it "reads 200mph" $ readSpeed "200mph" `shouldBe` milesPerHour 200
        it "reads 400kt" $ readSpeed "400kt" `shouldBe` knots 400
        it "reads 1ft/s" $ readSpeed "1ft/s" `shouldBe` feetPerSecond 1
    describe "Reading invalid speeds" $ do
        it "fails to read 5" $ readSpeedE "5" `shouldBe` Left "couldn't read speed 5"
        it "fails to read 5mps" $ readSpeedE "5mps" `shouldBe` Left "couldn't read speed 5mps"
    describe "Showing speeds" $
        it "shows speed in kilometres per hour" $
        show (kilometresPerHour 154) `shouldBe` "154.0km/h"
    describe "Converting speeds" $ do
        it "converts metres per seconds to kilometres per hour" $
            toKilometresPerHour (metresPerSecond 100) `shouldBe` 360.0
        it "converts metres per seconds to miles per hour" $
            toMilesPerHour (metresPerSecond 100) `shouldBe` 223.6936292054402
        it "converts kilometres per hour to knots" $
            toKnots (kilometresPerHour 1000) `shouldBe` 539.9568034557235
        it "converts feet per second to kilometres per hour" $
            toKilometresPerHour (feetPerSecond 1) `shouldBe` 1.09728
    describe "Resolution" $ do
        it "handles 1 km/h" $ toKilometresPerHour (kilometresPerHour 1) `shouldBe` 1
        it "handles 1 1m/s" $ toMetresPerSecond (metresPerSecond 1) `shouldBe` 1
        it "handles 1 1mph" $ toMilesPerHour (milesPerHour 1) `shouldBe` 1
        it "handles 1 knot" $ toKnots (knots 1) `shouldBe` 1
        it "handles 1 fp/s" $ toFeetPerSecond (feetPerSecond 1) `shouldBe` 1
    describe "Adding/Subtracting speeds" $ do
        it "adds speeds" $
            add (kilometresPerHour 1000) (metresPerSecond 1000) `shouldBe` kilometresPerHour 4600
        it "subtracts lengths" $
            sub (metresPerSecond 1000) (knots 10.5) `shouldBe` kilometresPerHour 3580.554
