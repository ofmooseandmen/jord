module Data.Geo.Jord.BearingSphericalSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
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
