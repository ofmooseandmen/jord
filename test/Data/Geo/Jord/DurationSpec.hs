module Data.Geo.Jord.DurationSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid durations" $ do
        it "reads 1H45M36.5S" $ readDuration "1H45M36.5S" `shouldBe` hms 1 45 36.5
        it "reads 45M" $ readDuration "45M" `shouldBe` minutes 45
        it "reads 36S" $ readDuration "36S" `shouldBe` seconds 36
        it "reads 36.6S" $ readDuration "36.6S" `shouldBe` milliseconds 36600
        it "read 1H-30M" $ readDuration "1H-30M" `shouldBe` hours 0.5
    describe "Reading invalid duration" $
        it "fails to read 5" $ readDurationE "5" `shouldBe` Left "couldn't read duration 5"
    describe "Showing duration" $
        it "shows duration" $ show (hms 1 45 36.5) `shouldBe` "1H45M36.500S"
    describe "Converting duration" $ do
        it "converts hours to seconds" $ toSeconds (hours 1) `shouldBe` 3600.0
        it "converts minutes to hours" $ toHours (minutes 30) `shouldBe` 0.5
        it "converts duration to milliseconds" $ toMilliseconds (hms 1 54 3.154) `shouldBe` 6843154
    describe "Adding/Subtracting duration" $ do
        it "adds duration" $ add (minutes 45) (seconds 36) `shouldBe` hms 0 45 36
        it "subtracts duration" $ sub (hours 1) (minutes 60) `shouldBe` zero
