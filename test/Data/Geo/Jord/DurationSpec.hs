module Data.Geo.Jord.DurationSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Duration as Duration

spec :: Spec
spec = do
    describe "Reading valid durations" $ do
        it "reads 1H45M36.5S" $ Duration.read "1H45M36.5S" `shouldBe` Just (Duration.hms 1 45 36.5)
        it "reads 45M" $ Duration.read "45M" `shouldBe` Just (Duration.minutes 45)
        it "reads 36S" $ Duration.read "36S" `shouldBe` Just (Duration.seconds 36)
        it "reads 36.6S" $ Duration.read "36.6S" `shouldBe` Just (Duration.milliseconds 36600)
        it "reads 1H-30M" $ Duration.read "1H-30M" `shouldBe` Just (Duration.hours 0.5)
        it "reads 0H8M5.953S" $
            Duration.read "0H8M5.953S" `shouldBe` Just (Duration.seconds 485.953)
    describe "Reading invalid duration" $
        it "fails to read 5" $ Duration.read "5" `shouldBe` Nothing
    describe "Showing duration" $
        it "shows duration" $ show (Duration.hms 1 45 36.5) `shouldBe` "1H45M36.500S"
    describe "Converting duration" $ do
        it "converts hours to seconds" $ Duration.toSeconds (Duration.hours 1) `shouldBe` 3600.0
        it "converts minutes to hours" $ Duration.toHours (Duration.minutes 30) `shouldBe` 0.5
        it "converts duration to milliseconds" $
            Duration.toMilliseconds (Duration.hms 1 54 3.154) `shouldBe` 6843154
    describe "Adding/Subtracting duration" $ do
        it "adds duration" $
            Duration.add (Duration.minutes 45) (Duration.seconds 36) `shouldBe` Duration.hms 0 45 36
        it "subtracts duration" $
            Duration.subtract (Duration.hours 1) (Duration.minutes 60) `shouldBe` Duration.zero
