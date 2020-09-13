module Data.Geo.Jord.AngleSpec
    ( spec
    ) where

import System.IO (hSetEncoding, stderr, stdin, stdout, utf8)

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Length as Length

spec :: Spec
spec = do
    describe "Reading valid angles" $ do
        it "reads 55°36'21\"" $
            Angle.read "55°36'21\"" `shouldBe` Just (Angle.decimalDegrees 55.6058333333)
        it "reads 55°36'21''" $
            Angle.read "55°36'21''" `shouldBe` Just (Angle.decimalDegrees 55.6058333333)
        it "reads 55d36m21.0s" $
            Angle.read "55d36m21.0s" `shouldBe` Just (Angle.decimalDegrees 55.6058333333)
        it "reads 55.6058333°" $
            Angle.read "55.6058333°" `shouldBe` Just (Angle.decimalDegrees 55.6058333)
        it "reads 55.6058333333°" $
            Angle.read "55.6058333333°" `shouldBe` Just (Angle.decimalDegrees 55.6058333333)
        it "reads -55.6058333333°" $
            Angle.read "-55.6058333333°" `shouldBe` Just (Angle.decimalDegrees (-55.6058333333))
        it "reads 96°01′18″" $ do
            hSetEncoding stdin utf8
            hSetEncoding stdout utf8
            hSetEncoding stderr utf8
            Angle.read "96°01′18″" `shouldBe` Just (Angle.decimalDegrees 96.02166666666)
    describe "Adding/Subtracting angles" $ do
        it "adds angles" $
            Angle.add (Angle.decimalDegrees 55.6058333) (Angle.decimalDegrees 5.0) `shouldBe`
            Angle.decimalDegrees 60.6058333
        it "subtracts angles" $
            Angle.subtract (Angle.decimalDegrees 5.0) (Angle.decimalDegrees 55.6058333) `shouldBe`
            Angle.decimalDegrees (-50.6058333)
    describe "Angle normalisation" $ do
        it "370 degrees normalised to [0..360] = 10" $
            Angle.normalise (Angle.decimalDegrees 370) (Angle.decimalDegrees 360) `shouldBe`
            Angle.decimalDegrees 10
        it "350 degrees normalised to [0..360] = 350" $
            Angle.normalise (Angle.decimalDegrees 350) (Angle.decimalDegrees 360) `shouldBe`
            Angle.decimalDegrees 350
    describe "Angle equality" $ do
        it "considers 59.9999999999° == 60.0°" $
            Angle.decimalDegrees 59.9999999999 `shouldBe` Angle.decimalDegrees 60
        it "considers 59.999999998° /= 60.0°" $
            Angle.decimalDegrees 59.999999998 `shouldNotBe` Angle.decimalDegrees 60
    describe "Showing angles" $ do
        it "shows 59.99999999999999 as 60°0'0.000\"" $
            show (Angle.decimalDegrees 59.99999999999999) `shouldBe` "60°0'0.000\""
        it "shows 154.915 as 154°54'54.000\"" $
            show (Angle.decimalDegrees 154.915) `shouldBe` "154°54'54.000\""
        it "shows -154.915 as -154°54'54.000\"" $
            show (Angle.decimalDegrees (-154.915)) `shouldBe` "-154°54'54.000\""
        it "show 0.5245 as 0°31'28.800\"" $
            show (Angle.decimalDegrees 0.5245) `shouldBe` "0°31'28.200\""
        it "show -0.5245 as -0°31'28.800\"" $
            show (Angle.decimalDegrees (-0.5245)) `shouldBe` "-0°31'28.200\""
    describe "Angle from decimal degrees" $ do
        it "returns 1 arcmillisecond when called with 1 / 3600000" $ do
            let actual = Angle.decimalDegrees (1 / 3600000)
            Angle.getDegrees actual `shouldBe` 0
            Angle.getArcminutes actual `shouldBe` 0
            Angle.getArcseconds actual `shouldBe` 0
            Angle.getArcmilliseconds actual `shouldBe` 1
        it "returns 1 arcsecond when called with 1000 / 3600000" $ do
            let actual = Angle.decimalDegrees (1000 / 3600000)
            Angle.getDegrees actual `shouldBe` 0
            Angle.getArcminutes actual `shouldBe` 0
            Angle.getArcseconds actual `shouldBe` 1
            Angle.getArcmilliseconds actual `shouldBe` 0
        it "returns 1 arcminute when called with 60000 / 3600000" $ do
            let actual = Angle.decimalDegrees (60000 / 3600000)
            Angle.getDegrees actual `shouldBe` 0
            Angle.getArcminutes actual `shouldBe` 1
            Angle.getArcseconds actual `shouldBe` 0
            Angle.getArcmilliseconds actual `shouldBe` 0
        it "returns 1 degree when called with 1" $ do
            let actual = Angle.decimalDegrees 1
            Angle.getDegrees actual `shouldBe` 1
            Angle.getArcminutes actual `shouldBe` 0
            Angle.getArcseconds actual `shouldBe` 0
            Angle.getArcmilliseconds actual `shouldBe` 0
        it "accepts positve values" $ do
            let actual = Angle.decimalDegrees 154.9150300
            Angle.getDegrees actual `shouldBe` 154
            Angle.getArcminutes actual `shouldBe` 54
            Angle.getArcseconds actual `shouldBe` 54
            Angle.getArcmilliseconds actual `shouldBe` 108
        it "accepts negative values" $ do
            let actual = Angle.decimalDegrees (-154.915)
            Angle.getDegrees actual `shouldBe` (-154)
            Angle.getArcminutes actual `shouldBe` 54
            Angle.getArcseconds actual `shouldBe` 54
            Angle.getArcmilliseconds actual `shouldBe` 0
    describe "Arc length" $ do
        it "computes the length of an arc with a central angle of 1 microarcsecond" $
            Angle.arcLength (Angle.decimalDegrees (1.0 / 3600000000.0)) (Length.kilometres 10000) `shouldBe`
            Length.metres 4.8e-5
        it "computes arc length with central angle of 0.6 microarcsecond" $
            Angle.arcLength (Angle.decimalDegrees (0.6 / 3600000000.0)) (Length.kilometres 10000) `shouldBe`
            Length.metres 4.8e-5
        it "computes arc length with central angle of 0.4 microarcsecond as 0" $
            Angle.arcLength (Angle.decimalDegrees (0.4 / 3600000000.0)) (Length.kilometres 1) `shouldBe`
            Length.zero
    describe "clockwiseDifference" $ do
        it "returns 0 if both angles are equal" $ do
            Angle.clockwiseDifference (Angle.decimalDegrees 154) (Angle.decimalDegrees 154) `shouldBe`
                Angle.zero
        it "return the difference between the 2 angles clockwise" $ do
            Angle.clockwiseDifference Angle.zero (Angle.decimalDegrees 10) `shouldBe`
                Angle.decimalDegrees 10
            Angle.clockwiseDifference Angle.zero (Angle.decimalDegrees (-10)) `shouldBe`
                Angle.decimalDegrees 350
            Angle.clockwiseDifference (Angle.decimalDegrees 350) (Angle.decimalDegrees 10) `shouldBe`
                Angle.decimalDegrees 20
            Angle.clockwiseDifference (Angle.decimalDegrees 350) (Angle.decimalDegrees 370) `shouldBe`
                Angle.decimalDegrees 20
