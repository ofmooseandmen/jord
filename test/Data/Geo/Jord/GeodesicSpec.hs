module Data.Geo.Jord.GeodesicSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Geodesic as Geodesic
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Model (Ellipsoidal)
import Data.Geo.Jord.Models (WGS84(..))

-- | See Geodesy Test Harness - latlon-ellipsoidal-vincenty  by Chris Veness.
-- https://github.com/chrisveness/geodesy/blob/master/test/latlon-ellipsoidal-vincenty-tests.js.
spec :: Spec
spec = do
    describe "Geodesic for (near) antipodal positions" $ do
        it "handles near-antipodal positions" $
            distance (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0.5 179.5 WGS84) `shouldBe`
            Just (Length.kilometres 19936.288578981)
        it "returns Nothing if vincenty fails to converge - inverseGeodesic" $
            Geodesic.inverse (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0.5 179.7 WGS84) `shouldBe`
            Nothing
        it "returns Nothing if vincenty fails to converge - distance" $
            distance (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0.5 179.7 WGS84) `shouldBe`
            Nothing
        it "returns Nothing if vincenty fails to converge - initialBearing" $
            initialBearing (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0.5 179.7 WGS84) `shouldBe`
            Nothing
        it "returns Nothing if vincenty fails to converge - finalBearing" $
            finalBearing (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0.5 179.7 WGS84) `shouldBe`
            Nothing
        it "handle antipodal positions - distance at equator" $
            distance (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0 180 WGS84) `shouldBe`
            Just (Length.kilometres 20003.931458623)
        it "handle antipodal positions - initialBearing at equator" $
            initialBearing (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0 180 WGS84) `shouldBe`
            Just Angle.zero
        it "handle antipodal positions - distance between poles" $
            distance (Geodetic.northPole WGS84) (Geodetic.southPole WGS84) `shouldBe`
            Just (Length.kilometres 20003.931458623)
        it "handle antipodal positions - initialBearing between poles" $
            initialBearing (Geodetic.northPole WGS84) (Geodetic.southPole WGS84) `shouldBe`
            Just Angle.zero
    describe "Geodesic for coincident positions" $ do
        let p = Geodetic.wgs84Pos 48 6
        it "returns a distance of 0 and no bearing" $ do
            let i = Geodesic.inverse p p
            let ib = i >>= Geodesic.initialBearing
            let fb = i >>= Geodesic.initialBearing
            fmap Geodesic.length i `shouldBe` Just Length.zero
            ib `shouldBe` Nothing
            fb `shouldBe` Nothing
        it "returns the given position when distance is 0" $
            destination p (Angle.decimalDegrees 54) Length.zero `shouldBe` Just p
    describe "Geodesic for selected positions" $ do
        let flindersPeak = Geodetic.latLongPos (-37.95103341666667) 144.42486788888888 WGS84
        let buninyong = Geodetic.latLongPos (-37.65282113888889) 143.92649552777777 WGS84
        let le = Geodetic.latLongPos 50.06632 (-5.71475) WGS84
        let jog = Geodetic.latLongPos 58.64402 (-3.07009) WGS84
        it "computes the surface distance - inverse" $ do
            distance flindersPeak buninyong `shouldBe` Just (Length.metres 54972.271139)
            distance le jog `shouldBe` Just (Length.kilometres 969.954166314)
        it "computes the initial bearing - inverse" $ do
            initialBearing flindersPeak buninyong `shouldBe`
                Just (Angle.decimalDegrees 306.86815920333333)
            initialBearing le jog `shouldBe` Just (Angle.decimalDegrees 9.14187748888889)
        it "computes the final bearing - inverse" $ do
            finalBearing flindersPeak buninyong `shouldBe`
                Just (Angle.decimalDegrees 307.17363062944446)
            finalBearing le jog `shouldBe` Just (Angle.decimalDegrees 11.297220414166667)
        it "compute the destination - direct" $ do
            destination
                flindersPeak
                (Angle.decimalDegrees 306.86815920333333)
                (Length.metres 54972.271139) `shouldBe`
                Just buninyong
            destination le (Angle.decimalDegrees 9.14187748888889) (Length.kilometres 969.954166314) `shouldBe`
                Just jog
        it "computes the final bearing - direct" $ do
            let fb1 =
                    Geodesic.direct
                        flindersPeak
                        (Angle.decimalDegrees 306.86815920333333)
                        (Length.metres 54972.271139) >>=
                    Geodesic.finalBearing
            fb1 `shouldBe` Just (Angle.decimalDegrees 307.17363062944446)
            let fb2 =
                    Geodesic.direct
                        le
                        (Angle.decimalDegrees 9.14187748888889)
                        (Length.kilometres 969.954166314) >>=
                    Geodesic.finalBearing
            fb2 `shouldBe` Just (Angle.decimalDegrees 11.297220414166667)
    describe "Surface distance for anti-meridian positions" $
        it "handles positions crossing antimeridian" $
        distance (Geodetic.latLongPos 30 120 WGS84) (Geodetic.latLongPos 30 (-120) WGS84) `shouldBe`
        Just (Length.kilometres 10825.924088908)
    describe "Geodesic for quadrants" $
        it "returns the same surface distance in all quadrants" $ do
            let actuals =
                    [ distance (Geodetic.latLongPos 30 30 WGS84) (Geodetic.latLongPos 60 60 WGS84)
                    , distance (Geodetic.latLongPos 60 60 WGS84) (Geodetic.latLongPos 30 30 WGS84)
                    , distance (Geodetic.latLongPos 30 60 WGS84) (Geodetic.latLongPos 60 30 WGS84)
                    , distance (Geodetic.latLongPos 60 30 WGS84) (Geodetic.latLongPos 30 60 WGS84)
                    , distance
                          (Geodetic.latLongPos 30 (-30) WGS84)
                          (Geodetic.latLongPos 60 (-60) WGS84)
                    , distance
                          (Geodetic.latLongPos 60 (-60) WGS84)
                          (Geodetic.latLongPos 30 (-30) WGS84)
                    , distance
                          (Geodetic.latLongPos 30 (-60) WGS84)
                          (Geodetic.latLongPos 60 (-30) WGS84)
                    , distance
                          (Geodetic.latLongPos 60 (-30) WGS84)
                          (Geodetic.latLongPos 30 (-60) WGS84)
                    , distance
                          (Geodetic.latLongPos (-30) (-30) WGS84)
                          (Geodetic.latLongPos (-60) (-60) WGS84)
                    , distance
                          (Geodetic.latLongPos (-60) (-60) WGS84)
                          (Geodetic.latLongPos (-30) (-30) WGS84)
                    , distance
                          (Geodetic.latLongPos (-30) (-60) WGS84)
                          (Geodetic.latLongPos (-60) (-30) WGS84)
                    , distance
                          (Geodetic.latLongPos (-60) (-30) WGS84)
                          (Geodetic.latLongPos (-30) (-60) WGS84)
                    , distance
                          (Geodetic.latLongPos (-30) 30 WGS84)
                          (Geodetic.latLongPos (-60) 60 WGS84)
                    , distance
                          (Geodetic.latLongPos (-60) 60 WGS84)
                          (Geodetic.latLongPos (-30) 30 WGS84)
                    , distance
                          (Geodetic.latLongPos (-30) 60 WGS84)
                          (Geodetic.latLongPos (-60) 30 WGS84)
                    , distance
                          (Geodetic.latLongPos (-60) 30 WGS84)
                          (Geodetic.latLongPos (-30) 60 WGS84)
                    ]
            let expecteds = replicate (length actuals) (Just (Length.kilometres 4015.703020938))
            actuals `shouldBe` expecteds
    describe "Inverse geodesic non-convergence" $ do
        it "returns Nothing for antipodal λ > π" $
            distance (Geodetic.latLongPos 0 0 WGS84) (Geodetic.latLongPos 0.5 179.7 WGS84) `shouldBe`
            Nothing
        it "returns Nothing for antipodal convergence" $
            distance (Geodetic.latLongPos 5 0 WGS84) (Geodetic.latLongPos (-5.1) 179.4 WGS84) `shouldBe`
            Nothing

finalBearing :: (Ellipsoidal a) => HorizontalPosition a -> HorizontalPosition a -> Maybe Angle
finalBearing p1 p2 = Geodesic.inverse p1 p2 >>= Geodesic.finalBearing

initialBearing :: (Ellipsoidal a) => HorizontalPosition a -> HorizontalPosition a -> Maybe Angle
initialBearing p1 p2 = Geodesic.inverse p1 p2 >>= Geodesic.initialBearing

distance :: (Ellipsoidal a) => HorizontalPosition a -> HorizontalPosition a -> Maybe Length
distance p1 p2 = fmap Geodesic.length (Geodesic.inverse p1 p2)

destination ::
       (Ellipsoidal a) => HorizontalPosition a -> Angle -> Length -> Maybe (HorizontalPosition a)
destination p b d = fmap Geodesic.endPosition (Geodesic.direct p b d)
