module Data.Geo.Jord.GeodesicSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord.Geodesic
import Data.Geo.Jord.Position

-- | See Geodesy Test Harness - latlon-ellipsoidal-vincenty  by Chris Veness.
-- https://github.com/chrisveness/geodesy/blob/master/test/latlon-ellipsoidal-vincenty-tests.js.
spec :: Spec
spec = do
    describe "Geodesic for (near) antipodal positions" $ do
        it "handles near-antipodal positions" $
            surfaceDistance (latLongPos 0 0 WGS84) (latLongPos 0.5 179.5 WGS84) `shouldBe`
            Just (kilometres 19936.288578981)
        it "returns Nothing if vincenty fails to converge - inverseGeodesic" $
            inverseGeodesic (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "returns Nothing if vincenty fails to converge - surfaceDistance" $
            surfaceDistance (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "returns Nothing if vincenty fails to converge - initialBearing" $
            initialBearing (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "returns Nothing if vincenty fails to converge - finalBearing" $
            finalBearing (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "handle antipodal positions - surfaceDistance at equator" $
            surfaceDistance (latLongPos 0 0 WGS84) (latLongPos 0 180 WGS84) `shouldBe` Just (kilometres 20003.931458623)
        it "handle antipodal positions - initialBearing at equator" $
            initialBearing (latLongPos 0 0 WGS84) (latLongPos 0 180 WGS84) `shouldBe` Just zero
        it "handle antipodal positions - surfaceDistance between poles" $
            surfaceDistance (northPole WGS84) (southPole WGS84) `shouldBe` Just (kilometres 20003.931458623)
        it "handle antipodal positions - initialBearing between poles" $
            initialBearing (northPole WGS84) (southPole WGS84) `shouldBe` Just zero
    describe "Geodesic for coincident positions" $ do
        let p = wgs84Pos 48 6 zero
        it "returns a distance of 0 and no bearing" $ do
            let i = inverseGeodesic p p
            let ib = i >>= geodesicBearing1
            let fb = i >>= geodesicBearing2
            fmap geodesicLength i `shouldBe` Just zero
            ib `shouldBe` Nothing
            fb `shouldBe` Nothing
            surfaceDistance p p `shouldBe` Just zero
            initialBearing p p `shouldBe` Nothing
            finalBearing p p `shouldBe` Nothing
        it "returns the given position when distance is 0" $ destination p (decimalDegrees 54) zero `shouldBe` Just p
    describe "Geodesic for selected positions" $ do
        let flindersPeak = latLongPos (-37.95103341666667) 144.42486788888888 WGS84
        let buninyong = latLongPos (-37.65282113888889) 143.92649552777777 WGS84
        let le = latLongPos 50.06632 (-5.71475) WGS84
        let jog = latLongPos 58.64402 (-3.07009) WGS84
        it "computes the surface distance - inverse" $ do
            surfaceDistance flindersPeak buninyong `shouldBe` Just (metres 54972.271139)
            surfaceDistance le jog `shouldBe` Just (kilometres 969.954166314)
        it "computes the initial bearing - inverse" $ do
            initialBearing flindersPeak buninyong `shouldBe` Just (decimalDegrees 306.86815920333333)
            initialBearing le jog `shouldBe` Just (decimalDegrees 9.14187748888889)
        it "computes the final bearing - inverse" $ do
            finalBearing flindersPeak buninyong `shouldBe` Just (decimalDegrees 307.17363062944446)
            finalBearing le jog `shouldBe` Just (decimalDegrees 11.297220414166667)
        it "compute the destination - direct" $ do
            destination flindersPeak (decimalDegrees 306.86815920333333) (metres 54972.271139) `shouldBe` Just buninyong
            destination le (decimalDegrees 9.14187748888889) (kilometres 969.954166314) `shouldBe` Just jog
        it "computes the final bearing - direct" $ do
            let fb1 =
                    directGeodesic flindersPeak (decimalDegrees 306.86815920333333) (metres 54972.271139) >>=
                    geodesicBearing2
            fb1 `shouldBe` Just (decimalDegrees 307.17363062944446)
            let fb2 =
                    directGeodesic le (decimalDegrees 9.14187748888889) (kilometres 969.954166314) >>= geodesicBearing2
            fb2 `shouldBe` Just (decimalDegrees 11.297220414166667)
    describe "Surface distance for anti-meridian positions" $
        it "handles positions crossing antimeridian" $
        surfaceDistance (latLongPos 30 120 WGS84) (latLongPos 30 (-120) WGS84) `shouldBe`
        Just (kilometres 10825.924088908)
    describe "Geodesic for quadrants" $
        it "returns the same surface distance in all quadrants" $ do
            let actuals =
                    [ surfaceDistance (latLongPos 30 30 WGS84) (latLongPos 60 60 WGS84)
                    , surfaceDistance (latLongPos 60 60 WGS84) (latLongPos 30 30 WGS84)
                    , surfaceDistance (latLongPos 30 60 WGS84) (latLongPos 60 30 WGS84)
                    , surfaceDistance (latLongPos 60 30 WGS84) (latLongPos 30 60 WGS84)
                    , surfaceDistance (latLongPos 30 (-30) WGS84) (latLongPos 60 (-60) WGS84)
                    , surfaceDistance (latLongPos 60 (-60) WGS84) (latLongPos 30 (-30) WGS84)
                    , surfaceDistance (latLongPos 30 (-60) WGS84) (latLongPos 60 (-30) WGS84)
                    , surfaceDistance (latLongPos 60 (-30) WGS84) (latLongPos 30 (-60) WGS84)
                    , surfaceDistance (latLongPos (-30) (-30) WGS84) (latLongPos (-60) (-60) WGS84)
                    , surfaceDistance (latLongPos (-60) (-60) WGS84) (latLongPos (-30) (-30) WGS84)
                    , surfaceDistance (latLongPos (-30) (-60) WGS84) (latLongPos (-60) (-30) WGS84)
                    , surfaceDistance (latLongPos (-60) (-30) WGS84) (latLongPos (-30) (-60) WGS84)
                    , surfaceDistance (latLongPos (-30) 30 WGS84) (latLongPos (-60) 60 WGS84)
                    , surfaceDistance (latLongPos (-60) 60 WGS84) (latLongPos (-30) 30 WGS84)
                    , surfaceDistance (latLongPos (-30) 60 WGS84) (latLongPos (-60) 30 WGS84)
                    , surfaceDistance (latLongPos (-60) 30 WGS84) (latLongPos (-30) 60 WGS84)
                    ]
            let expecteds = replicate (length actuals) (Just (kilometres 4015.703020938))
            actuals `shouldBe` expecteds
    describe "Inverse geodesic non-convergence" $ do
        it "returns Nothing for antipodal λ > π" $
            surfaceDistance (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84) `shouldBe` Nothing
        it "returns Nothing for antipodal convergence" $
            surfaceDistance (latLongPos 5 0 WGS84) (latLongPos (-5.1) 179.4 WGS84) `shouldBe` Nothing