module Data.Geo.Jord.GeodesicSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord.Geodesic
import Data.Geo.Jord.Position

-- | See Geodesy Test Harness - latlon-ellipsoidal-vincenty  by Chris Veness - TODO link
spec :: Spec
spec = do
    describe "Geodesic for (near) antipodal positions" $ do
        it "handles near-antipodal positions" $ do
            surfaceDistance (latLongPos 0 0 WGS84) (latLongPos 0.5 179.5 WGS84) `shouldBe`
                Just (kilometres 19936.288578981)
        it "returns Nothing if vincenty fails to converge - inverseGeodesic" $ do
            inverseGeodesic (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "returns Nothing if vincenty fails to converge - surfaceDistance" $ do
            surfaceDistance (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "returns Nothing if vincenty fails to converge - initialBearing" $ do
            initialBearing (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "returns Nothing if vincenty fails to converge - finalBearing" $ do
            finalBearing (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "handle antipodal positions - surfaceDistance at equator" $ do
            surfaceDistance (latLongPos 0 0 WGS84) (latLongPos 0 180 WGS84) `shouldBe`
                Just (kilometres 20003.931458623)
        it "handle antipodal positions - initialBearing at equator" $ do
            initialBearing (latLongPos 0 0 WGS84) (latLongPos 0 180 WGS84) `shouldBe` Just zero
        it "handle antipodal positions - surfaceDistance between poles" $ do
            surfaceDistance (northPole WGS84) (southPole WGS84) `shouldBe`
                Just (kilometres 20003.931458623)
        it "handle antipodal positions - initialBearing between poles" $ do
            initialBearing (northPole WGS84) (southPole WGS84) `shouldBe` Just zero
    describe "Geodesic for selected positions" $ do
        let flindersPeak = latLongPos (-37.95103341666667) 144.42486788888888 WGS84
        let buninyong = latLongPos (-37.65282113888889) 143.92649552777777 WGS84
        let le = latLongPos 50.06632 (-5.71475) WGS84
        let jog = latLongPos 58.64402 (-3.07009) WGS84
        it "computes the surface distance" $ do
            surfaceDistance flindersPeak buninyong `shouldBe` Just (metres 54972.271139)
            surfaceDistance le jog `shouldBe` Just (kilometres 969.954166314)
        it "computes the initial bearing" $ do
            initialBearing flindersPeak buninyong `shouldBe`
                Just (decimalDegrees 306.86815920333333)
            initialBearing le jog `shouldBe` Just (decimalDegrees 9.14187748888889)
        it "computes the final bearing" $ do
            finalBearing flindersPeak buninyong `shouldBe` Just (decimalDegrees 307.17363062944446)
            finalBearing le jog `shouldBe` Just (decimalDegrees 11.297220414166667)
