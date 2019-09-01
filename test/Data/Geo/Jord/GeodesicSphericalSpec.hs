module Data.Geo.Jord.GeodesicSphericalSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "destination" $ do
        it "return the given position if distance is 0 meter" $ do
            let p0 = s84Pos 53.320556 (-1.729722) zero
            destination p0 (decimalDegrees 96.0217) zero `shouldBe` p0
        it "return the position along great-circle at distance and bearing (llh)" $ do
            let p0 = s84Pos 53.320556 (-1.729722) (metres 15000.0)
            let p1 = s84Pos 53.1882697 0.1332747 (metres 15000.0)
            destination p0 (decimalDegrees 96.0217) (metres 124800) `shouldBe` p1
        it "return the position along great-circle at distance and bearing (ECEF)" $ do
            let p0 = ecefMetresPos 3812864.094 (-115142.863) 5121515.161 S84
            let p1 = ecefMetresPos 3826406.471 8900.5364 5112694.2331 S84
            destination p0 (decimalDegrees 96.0217) (metres 124800) `shouldBe` p1
    describe "finalBearing" $ do
        it "returns the Nothing if both positions are the same (ignoring height)" $ do
            let p = s84Pos 50.066389 (-5.714722) zero
            finalBearing p p `shouldBe` Nothing
            finalBearing p (s84Pos 50.066389 (-5.714722) (metres 10)) `shouldBe` Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 12000)
            let p2 = s84Pos 58.643889 (-5.714722) (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = s84Pos 58.643889 (-5.714722) (metres 5000)
            let p2 = s84Pos 50.066389 (-5.714722) (metres 12000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = s84Pos 0 0 (metres 12000)
            let p2 = s84Pos 0 1 (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = s84Pos 0 1 (metres 12000)
            let p2 = s84Pos 0 0 (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 270)
        it "returns the final bearing in compass angle" $ do
            let p1 = s84Pos 50.066389 (-5.714722) zero
            let p2 = s84Pos 58.643889 (-3.07) zero
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 11.2752)
        it "returns the final bearing in compass angle" $ do
            let p1 = s84Pos 58.643889 (-3.07) zero
            let p2 = s84Pos 50.066389 (-5.714722) zero
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 189.1198172)
        it "returns the final bearing in compass angle" $ do
            let p1 = s84Pos (-53.994722) (-25.9875) zero
            let p2 = s84Pos 54 154 zero
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 125.6853725)
    describe "initialBearing" $ do
        it "returns Nothing if both positions are the same (ignoring height)" $ do
            let p = s84Pos 50.066389 (-179.999722) zero
            initialBearing p p `shouldBe` Nothing
            initialBearing p (s84Pos 50.066389 (-179.999722) (metres 100)) `shouldBe` Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 12000)
            let p2 = s84Pos 58.643889 (-5.714722) (metres 12000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = s84Pos 58.643889 (-5.714722) (metres 12000)
            let p2 = s84Pos 50.066389 (-5.714722) (metres 12000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = s84Pos 0 0 (metres 12000)
            let p2 = s84Pos 0 1 (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = s84Pos 0 1 (metres 12000)
            let p2 = s84Pos 0 0 (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 270)
        it "returns 0° at the prime meridian going north" $ do
            let p1 = s84Pos 50 0 zero
            let p2 = s84Pos 58 0 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° at the prime meridian going south" $ do
            let p1 = s84Pos 58 0 zero
            let p2 = s84Pos 50 0 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 0° at the date line going north" $ do
            let p1 = s84Pos 50 180 zero
            let p2 = s84Pos 58 180 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° at the date line going south" $ do
            let p1 = s84Pos 58 180 zero
            let p2 = s84Pos 50 180 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 0° going from the south pole to the north pole" $ do
            let p1 = southPole S84
            let p2 = northPole S84
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 0° going from the north pole to the south pole" $ do
            let p1 = northPole S84
            let p2 = southPole S84
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 0° going from the south pole to anywhere on the date line" $ do
            let p1 = southPole S84
            let p2 = s84Pos 50 180 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns the initial bearing in compass angle" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 12000)
            let p2 = s84Pos 58.643889 (-3.07) (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 9.1198172)
        it "returns the initial bearing in compass angle" $ do
            let p1 = s84Pos 58.643889 (-3.07) (metres 12000)
            let p2 = s84Pos 50.066389 (-5.714722) (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 191.2752)
    describe "surfaceDistance" $ do
        it "returns 0 if both points are equal" $ do
            let p = s84Pos 50.066389 (-5.714722) (metres 15000.0)
            surfaceDistance p p `shouldBe` zero
        it "returns the distance between 2 points" $ do
            let p1 = s84Pos 50.066389 (-5.714722) zero
            let p2 = s84Pos 58.643889 (-3.07) zero
            surfaceDistance p1 p2 `shouldBe` metres 968854.8685
        it "handles singularity at the pole" $
            surfaceDistance (northPole S84) (southPole S84) `shouldBe` kilometres 20015.114352200002
        it "handles the discontinuity at the Date Line" $ do
            let p1 = s84Pos 50.066389 (-179.999722) zero
            let p2 = s84Pos 50.066389 179.999722 zero
            surfaceDistance p1 p2 `shouldBe` metres 39.6905