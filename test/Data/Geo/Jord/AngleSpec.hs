module Data.Geo.Jord.AngleSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid angles" $ do
        it "reads 55°36'21''" $ readAngle "55°36'21''" `angleShouldBe` ofDegrees 55.6058333
        it "reads 55.6058333°" $ readAngle "55.6058333°" `angleShouldBe` ofDegrees 55.6058333
        it "reads -55.6058333°" $ readAngle "-55.6058333°" `angleShouldBe` ofDegrees (-55.6058333)
    describe "Adding/Subtracting angles" $ do
        it "adds angles" $ add (ofDegrees 55.6058333) (ofDegrees 5.0) `angleShouldBe` ofDegrees 60.6058333
        it "subtracts angles" $ sub (ofDegrees 5.0) (ofDegrees 55.6058333) `angleShouldBe` ofDegrees (-50.6058333)
    describe "degrees/radians conversion" $ do
        it "converts degrees to radians" $ toRadians 55.6058333 `shouldBe` 0.9705048744001038
        it "converts radians to degrees" $ toDegrees 2.6436343 `shouldBe` 151.46908796602173
    describe "angle normalisation" $ do
        it "it normalises radians in given range" $ normalise (ofRadians (3.0 * pi / 2.0)) 180.0 `angleShouldBe` ofDegrees 90.0
        it "it normalises degrees in given range" $ normalise (ofDegrees 371.1) 360.0 `angleShouldBe` ofDegrees 11.1
