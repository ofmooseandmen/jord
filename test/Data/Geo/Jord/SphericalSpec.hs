module Data.Geo.Jord.SphericalSpec
    ( spec
    ) where

import Control.Exception.Base
import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Antipode" $ do
        it "returns the antipodal point" $ do
            let p = latLongPos (readLatLong "484137N0061105E") 15000
            let e = latLongPos (latLongDecimal (-48.6936111) (-173.8152777)) 15000
            antipode p `shouldBe` e
        it "returns the south pole when called with the north pole" $
            antipode (fromNVector northPole 0.0) `shouldBe` latLongDecimal (-90.0) (-180.0)
        it "returns the north pole when called with the south pole" $
            antipode (fromNVector southPole 0.0) `shouldBe` latLongDecimal 90.0 (-180.0)
    describe "Surface Distance" $ do
        it "returns 0 if both points are equal" $ do
            let p = readLatLong "500359N1795959W"
            surfaceDistance p p (meanRadius wgs84) `shouldBe` metres 0.0
        it "returns the distance between 2 points" $ do
            let p1 = readLatLong "500359N0054253W"
            let p2 = readLatLong "583838N0030412W"
            surfaceDistance p1 p2 (meanRadius wgs84) `shouldBe` metres 968854.868
        it "handles singularity at the pole" $
            surfaceDistance northPole southPole (meanRadius wgs84) `shouldBe`
                kilometres 20015.114351
        it "handles the discontinuity at the Date Line" $ do
            let p1 = readLatLong "500359N1795959W"
            let p2 = readLatLong "500359N1795959E"
            surfaceDistance p1 p2 (meanRadius wgs84) `shouldBe` metres 39.66
    describe "Initial bearing" $ do
        it "returns the 0 if both point are the same" $ do
            let p = readLatLong "500359N1795959W"
            initialBearing p p `shouldBe` decimalDegrees 0
        it "returns the initial bearing in compass angle" $ do
            let p1 = latLongPos (readLatLong "500359N0054253W") 12000
            let p2 = latLongPos (readLatLong "583838N0030412W") 5000
            initialBearing p1 p2 `shouldBe` decimalDegrees 9.1198181
        it "returns the initial bearing in compass angle" $ do
            let p1 = latLongPos (readLatLong "583838N0030412W") 12000
            let p2 = latLongPos (readLatLong "500359N0054253W") 5000
            initialBearing p1 p2 `shouldBe` decimalDegrees 191.2752013
    describe "Interpolate" $ do
        let p1 = readLatLong "44N044E"
        let p2 = readLatLong "46N046E"
        it "fails if f < 0.0" $
            evaluate (interpolate p1 p2 (-0.5)) `shouldThrow`
            errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (interpolate p1 p2 1.1) `shouldThrow`
            errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $ interpolate p1 p2 0.0 `shouldBe` p1
        it "returns p1 if f == 1" $ interpolate p1 p2 1.0 `shouldBe` p2
        it "returns the interpolated position" $ do
            let p3 = latLongPos (readLatLong "53°28'46''N 2°14'43''W") 10000
            let p4 = latLongPos (readLatLong "55°36'21''N 13°02'09''E") 20000
            interpolate p3 p4 0.5 `shouldBe` latLongPos (latLongDecimal 54.7835574 5.1949856) 15000
    describe "insideSurface" $ do
        let p1 = latLongDecimal 45 1
        let p2 = latLongDecimal 45 2
        let p3 = latLongDecimal 46 1
        let p4 = latLongDecimal 46 2
        let p5 = latLongDecimal 45.1 1.1
        it "return False if polygon is empty" $ insideSurface p1 [] `shouldBe` False
        it "return False if polygon does not define at least a triangle" $
            insideSurface p1 [p1, p2] `shouldBe` False
        it "returns True if point is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            insideSurface p5 polygon `shouldBe` True
        it "returns False if point is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            let p = antipode p5
            insideSurface p polygon `shouldBe` False
        it "returns False if point is a vertex of the polygon" $ do
            let polygon = [p1, p2, p4, p3]
            insideSurface p1 polygon `shouldBe` False
        it "handles closed polygons" $ do
            let polygon = [p1, p2, p4, p3, p1]
            insideSurface p5 polygon `shouldBe` True
        it "handles concave polygons" $ do
            let malmo = latLongDecimal 55.6050 13.0038
            let ystad = latLongDecimal 55.4295 13.82
            let lund = latLongDecimal 55.7047 13.1910
            let helsingborg = latLongDecimal 56.0465 12.6945
            let kristianstad = latLongDecimal 56.0294 14.1567
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            let hoor = latLongDecimal 55.9295 13.5297
            let hassleholm = latLongDecimal 56.1589 13.7668
            insideSurface hoor polygon `shouldBe` True
            insideSurface hassleholm polygon `shouldBe` False
    describe "Final bearing" $ do
        it "returns the 180.0 if both point are the same" $ do
            let p = readLatLong "500359N0054253W"
            finalBearing p p `shouldBe` decimalDegrees 180
        it "returns the final bearing in compass angle" $ do
            let p1 = readLatLong "500359N0054253W"
            let p2 = readLatLong "583838N0030412W"
            finalBearing p1 p2 `shouldBe` decimalDegrees 11.2752013
        it "returns the final bearing in compass angle" $ do
            let p1 = readLatLong "583838N0030412W"
            let p2 = readLatLong "500359N0054253W"
            finalBearing p1 p2 `shouldBe` decimalDegrees 189.1198181
        it "returns the final bearing in compass angle" $ do
            let p1 = readLatLong "535941S0255915W"
            let p2 = readLatLong "54N154E"
            finalBearing p1 p2 `shouldBe` decimalDegrees 125.6839436
    describe "Mean" $ do
        it "returns Nothing if no point is given" $ (mean [] :: Maybe NVector) `shouldBe` Nothing
        it "returns the unique given point" $ do
            let p = readLatLong "500359N0054253W"
            mean [p] `shouldBe` Just p
        it "returns the geographical mean" $ do
            let p1 = latLongPos (readLatLong "500359N0054253W") 15000
            let p2 = latLongPos (readLatLong "583838N0030412W") 25000
            let e = latLongPos (latLongDecimal 54.3622869 (-4.5306725)) 0
            mean [p1, p2] `shouldBe` Just e
        it "returns Nothing if list contains antipodal points" $ do
            let points =
                    [ latLongDecimal 45 1
                    , latLongDecimal 45 2
                    , latLongDecimal 46 2
                    , latLongDecimal 46 1
                    , antipode (latLongDecimal 45 2)
                    ]
            mean points `shouldBe` Nothing
