module Data.Geo.Jord.AngleSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid angles" $ do
        it "reads 55°36'21\"" $ readAngle "55°36'21\"" `shouldBe` decimalDegrees 55.6058333
        it "reads 55°36'21''" $ readAngle "55°36'21''" `shouldBe` decimalDegrees 55.6058333
        it "reads 55d36m21.0s" $ readAngle "55d36m21.0s" `shouldBe` decimalDegrees 55.6058333
        it "reads 55.6058333°" $ readAngle "55.6058333°" `shouldBe` decimalDegrees 55.6058333
        it "reads -55.6058333°" $ readAngle "-55.6058333°" `shouldBe` decimalDegrees (-55.6058333)
        it "reads 96°01′18″" $ readAngle "96°01′18″" `shouldBe` decimalDegrees 96.02166666
    describe "Adding/Subtracting angles" $ do
        it "adds angles" $
            add (decimalDegrees 55.6058333) (decimalDegrees 5.0) `shouldBe`
            decimalDegrees 60.6058333
        it "subtracts angles" $
            sub (decimalDegrees 5.0) (decimalDegrees 55.6058333) `shouldBe`
            decimalDegrees (-50.6058333)
    describe "Angle normalisation" $ do
        it "370 degrees normalised to [0..360] = 10" $
            normalise (decimalDegrees 370) (decimalDegrees 360) `shouldBe` decimalDegrees 10
        it "350 degrees normalised to [0..360] = 350" $
            normalise (decimalDegrees 350) (decimalDegrees 360) `shouldBe` decimalDegrees 350
    describe "Angle equality" $ do
        it "considers 59.9999999° == 60.0°" $ decimalDegrees 59.9999999 `shouldBe` decimalDegrees 60
        it "considers 59.9999998° /= 60.0°" $
            decimalDegrees 59.9999998 `shouldNotBe` decimalDegrees 60
    describe "Showing angles" $ do
        it "shows 59.99999999999999 as 60°0'0.0\"" $
            show (decimalDegrees 59.99999999999999) `shouldBe` "60°0'0.0\""
        it "shows 154.915 as 154°54'54.0\"" $
            show (decimalDegrees 154.915) `shouldBe` "154°54'54.0\""
        it "shows -154.915 as -154°54'54.0\"" $
            show (decimalDegrees (-154.915)) `shouldBe` "-154°54'54.0\""
    describe "Angle from decimal degrees" $ do
        it "returns 1 millisecond when called with 1 / 3600000" $ do
            let actual = decimalDegrees (1 / 3600000)
            getDegrees actual `shouldBe` 0
            getMinutes actual `shouldBe` 0
            getSeconds actual `shouldBe` 0
            getMilliseconds actual `shouldBe` 1
        it "returns 1 second when called with 1000 / 3600000" $ do
            let actual = decimalDegrees (1000 / 3600000)
            getDegrees actual `shouldBe` 0
            getMinutes actual `shouldBe` 0
            getSeconds actual `shouldBe` 1
            getMilliseconds actual `shouldBe` 0
        it "returns 1 minute when called with 60000 / 3600000" $ do
            let actual = decimalDegrees (60000 / 3600000)
            getDegrees actual `shouldBe` 0
            getMinutes actual `shouldBe` 1
            getSeconds actual `shouldBe` 0
            getMilliseconds actual `shouldBe` 0
        it "returns 1 degree when called with 1" $ do
            let actual = decimalDegrees 1
            getDegrees actual `shouldBe` 1
            getMinutes actual `shouldBe` 0
            getSeconds actual `shouldBe` 0
            getMilliseconds actual `shouldBe` 0
        it "accepts positve values" $ do
            let actual = decimalDegrees 154.9150300
            getDegrees actual `shouldBe` 154
            getMinutes actual `shouldBe` 54
            getSeconds actual `shouldBe` 54
            getMilliseconds actual `shouldBe` 108
        it "accepts negative values" $ do
            let actual = decimalDegrees (-154.915)
            getDegrees actual `shouldBe` (-154)
            getMinutes actual `shouldBe` 54
            getSeconds actual `shouldBe` 54
            getMilliseconds actual `shouldBe` 0
    describe "Arc length" $ do
        it "computes the length of an arc with a central angle of 1 milliseconds" $
            arcLength (decimalDegrees (1.0 / 3600000.0)) meanEarthRadius `shouldBe` metres 0.031
        it
            "arc length with central angle of 0.6 milliseconds == arc length with central angle of 1 milliseconds" $
            arcLength (decimalDegrees (0.6 / 3600000.0)) meanEarthRadius `shouldBe` metres 0.031
        it "arc length with central angle of 0.5 milliseconds == 0" $
            arcLength (decimalDegrees (0.4 / 3600000.0)) meanEarthRadius `shouldBe` metres 0
