module Data.Geo.Jord.SpeedSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Speed

spec :: Spec
spec = do
    describe "Reading valid speeds" $ do
        it "reads -15.2m/s" $ readSpeed "-15.2m/s" `shouldBe` Just (metresPerSecond (-15.2))
        it "reads 154km/h" $ readSpeed "154km/h" `shouldBe` Just (kilometresPerHour 154)
        it "reads 200mph" $ readSpeed "200mph" `shouldBe` Just (milesPerHour 200)
        it "reads 400kt" $ readSpeed "400kt" `shouldBe` Just (knots 400)
        it "reads 1ft/s" $ readSpeed "1ft/s" `shouldBe` Just (feetPerSecond 1)
    describe "Reading invalid speeds" $ do
        it "fails to read 5" $ readSpeed "5" `shouldBe` Nothing
        it "fails to read 5mps" $ readSpeed "5mps" `shouldBe` Nothing
    describe "Showing speeds" $
        it "shows speed in kilometres per hour" $
        show (kilometresPerHour 154) `shouldBe` "154.0km/h"
    describe "Converting speeds" $ do
        it "converts metres per seconds to kilometres per hour" $
            toKilometresPerHour (metresPerSecond 100) `shouldBe` 360.0
        it "converts metres per seconds to miles per hour" $
            toMilesPerHour (metresPerSecond 100) `shouldBe` 223.69362920544023
        it "converts kilometres per hour to knots" $
            toKnots (kilometresPerHour 1000) `shouldBe` 539.9568034557235
        it "converts feet per second to kilometres per hour" $
            toKilometresPerHour (feetPerSecond 1) `shouldBe` 1.09728
    describe "Resolution" $ do
        it "handles 1 km/h" $ toKilometresPerHour (kilometresPerHour 1) `shouldBe` 1
        it "handles 1 m/s" $ toMetresPerSecond (metresPerSecond 1) `shouldBe` 1
        it "handles 1 mph" $ toMilesPerHour (milesPerHour 1) `shouldBe` 1
        it "handles 1 knot" $ toKnots (knots 1) `shouldBe` 1
        it "handles 1 fp/s" $ toFeetPerSecond (feetPerSecond 1) `shouldBe` 1
    describe "Adding/Subtracting speeds" $ do
        it "adds speeds" $
            add (kilometresPerHour 1000) (metresPerSecond 1000) `shouldBe` kilometresPerHour 4600
        it "subtracts speeds" $
            sub (metresPerSecond 1000) (knots 10.5) `shouldBe` kilometresPerHour 3580.554
