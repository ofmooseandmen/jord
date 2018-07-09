module Data.Geo.Jord.AngleSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid angles" $ do
        it "reads 55°36'21''" $ readAngle "55°36'21''" `shouldBe` decimalDegrees 55.6058333
        it "reads 55.6058333°" $ readAngle "55.6058333°" `shouldBe` decimalDegrees 55.6058333
        it "reads -55.6058333°" $ readAngle "-55.6058333°" `shouldBe` decimalDegrees (-55.6058333)
    describe "Adding/Subtracting angles" $ do
        it "adds angles" $ add (decimalDegrees 55.6058333) (decimalDegrees 5.0) `shouldBe` decimalDegrees 60.6058333
        it "subtracts angles" $
            sub (decimalDegrees 5.0) (decimalDegrees 55.6058333) `shouldBe` decimalDegrees (-50.6058333)
    describe "angle normalisation" $ do
        it "normalises degrees in given range" $
            normalise (decimalDegrees 371.1) 360 `shouldBe` decimalDegrees 11.1
    describe "angle equality" $ do
        it "considers 59.99999995° == 60.0°" $ decimalDegrees 59.99999995 `shouldBe` decimalDegrees 60
        it "considers 59.99999994° /= 60.0°" $ decimalDegrees 59.99999994 `shouldNotBe` decimalDegrees 60
    describe "Showing angles" $ do
        it "shows 59.99999999999999 as 60°0'0.0\"" $
            show (decimalDegrees 59.99999999999999) `shouldBe` "60°0'0.0\""
