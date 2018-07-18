module Data.Geo.Jord.Spherical.GeodeticsSpec
    ( spec
    ) where

import Control.Exception.Base
import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Antipode" $ do
        it "returns the antipodal point" $
            antipode (readLatLong "484137N0061105E") `shouldBe`
            latLongDecimal (-48.6936111) (-173.8152777)
        it "returns the south pole when called with the north pole" $
            antipode (northPole :: LatLong) `shouldBe` latLongDecimal (-90.0) (-180.0)
        it "returns the north pole when called with the south pole" $
            antipode (southPole :: LatLong) `shouldBe` latLongDecimal 90.0 (-180.0)
    describe "Distance" $ do
        it "returns 0 if both points are equal" $
            distance (readLatLong "500359N1795959W") (readLatLong "500359N1795959W") `shouldBe`
            metres 0.0
        it "returns the distance between 2 points" $
            distance (readLatLong "500359N0054253W") (readLatLong "583838N0030412W") `shouldBe`
            metres 968854.868
        it "handles singularity at the pole" $
            distance (northPole :: LatLong) (southPole :: LatLong) `shouldBe`
            kilometres 20015.114351
        it "handles the discontinuity at the Date Line" $
            distance (readLatLong "500359N1795959W") (readLatLong "500359N1795959E") `shouldBe`
            metres 39.66
    describe "Destination" $ do
        it "return the given point if distance is 0 meter" $
            destination (readLatLong "531914N0014347W") (decimalDegrees 96.0217) (metres 0) `shouldBe`
            readLatLong "531914N0014347W"
        it "return the destination point along great-circle at distance and bearing" $
            destination (readLatLong "531914N0014347W") (decimalDegrees 96.0217) (metres 124800) `shouldBe`
            latLongDecimal 53.1882691 0.1332744
    describe "Initial bearing" $ do
        it "returns the 0 if both point are the same" $
            initialBearing (readLatLong "500359N0054253W") (readLatLong "500359N0054253W") `shouldBe`
            decimalDegrees 0
        it "returns the initial bearing in compass degrees" $
            initialBearing (readLatLong "500359N0054253W") (readLatLong "583838N0030412W") `shouldBe`
            decimalDegrees 9.1198181
        it "returns the initial bearing in compass degrees" $
            initialBearing (readLatLong "583838N0030412W") (readLatLong "500359N0054253W") `shouldBe`
            decimalDegrees 191.2752013
    describe "Interpolate" $ do
        it "fails if f < 0.0" $
            evaluate (interpolate (readLatLong "44N044E") (readLatLong "46N046E") (-0.5)) `shouldThrow`
            errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (interpolate (readLatLong "44N044E") (readLatLong "46N046E") 1.1) `shouldThrow`
            errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $
            interpolate (readLatLong "44N044E") (readLatLong "46N046E") 0.0 `shouldBe`
            readLatLong "44N044E"
        it "returns p1 if f == 1" $
            interpolate (readLatLong "44N044E") (readLatLong "46N046E") 1.0 `shouldBe`
            readLatLong "46N046E"
        it "returns the interpolated position" $
            interpolate
                (readLatLong "53°28'46''N 2°14'43''W")
                (readLatLong "55°36'21''N 13°02'09''E")
                0.5 `shouldBe`
            latLongDecimal 54.7835574 5.1949856
    describe "isInside" $ do
        it "return False if polygon is empty" $ isInside (latLongDecimal 45 1) [] `shouldBe` False
        it "return False if polygon does not define at least a triangle" $
            isInside (latLongDecimal 45 1) [latLongDecimal 45 1, latLongDecimal 45 2] `shouldBe`
            False
        it "returns True if point is inside polygon" $ do
            let polygon =
                    [ latLongDecimal 45 1
                    , latLongDecimal 45 2
                    , latLongDecimal 46 2
                    , latLongDecimal 46 1
                    ]
            let p = latLongDecimal 45.1 1.1
            isInside p polygon `shouldBe` True
        it "returns False if point is inside polygon" $ do
            let polygon =
                    [ latLongDecimal 45 1
                    , latLongDecimal 45 2
                    , latLongDecimal 46 2
                    , latLongDecimal 46 1
                    ]
            let p = antipode (latLongDecimal 45.1 1.1)
            isInside p polygon `shouldBe` False
        it "returns False if point is a vertex of the polygon" $ do
            let polygon =
                    [ latLongDecimal 45 1
                    , latLongDecimal 45 2
                    , latLongDecimal 46 2
                    , latLongDecimal 46 1
                    ]
            let p = latLongDecimal 45 1
            isInside p polygon `shouldBe` False
        it "handles closed polygons" $ do
            let polygon =
                    [ latLongDecimal 45 1
                    , latLongDecimal 45 2
                    , latLongDecimal 46 2
                    , latLongDecimal 46 1
                    , latLongDecimal 45 1
                    ]
            let p = latLongDecimal 45.1 1.1
            isInside p polygon `shouldBe` True
        it "handles concave polygons" $ do
            let malmo = latLongDecimal 55.6050 13.0038
            let ystad = latLongDecimal 55.4295 13.82
            let lund = latLongDecimal 55.7047 13.1910
            let helsingborg = latLongDecimal 56.0465 12.6945
            let kristianstad = latLongDecimal 56.0294 14.1567
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            let hoor = latLongDecimal 55.9295 13.5297
            let hassleholm = latLongDecimal 56.1589 13.7668
            isInside hoor polygon `shouldBe` True
            isInside hassleholm polygon `shouldBe` False
    describe "Final bearing" $ do
        it "returns the 180.0 if both point are the same" $
            finalBearing (readLatLong "500359N0054253W") (readLatLong "500359N0054253W") `shouldBe`
            decimalDegrees 180
        it "returns the final bearing in compass degrees" $
            finalBearing (readLatLong "500359N0054253W") (readLatLong "583838N0030412W") `shouldBe`
            decimalDegrees 11.2752013
        it "returns the final bearing in compass degrees" $
            finalBearing (readLatLong "583838N0030412W") (readLatLong "500359N0054253W") `shouldBe`
            decimalDegrees 189.1198181
        it "returns the final bearing in compass degrees" $
            finalBearing (readLatLong "535941S0255915W") (readLatLong "54N154E") `shouldBe`
            decimalDegrees 125.6839436
    describe "Mean" $ do
        it "returns Nothing if no point is given" $ (mean [] :: Maybe LatLong) `shouldBe` Nothing
        it "returns the unique given point" $
            mean [readLatLong "500359N0054253W"] `shouldBe` Just (readLatLong "500359N0054253W")
        it "returns the geographical mean" $
            mean [readLatLong "500359N0054253W", readLatLong "583838N0030412W"] `shouldBe`
            Just (latLongDecimal 54.3622869 (-4.5306725))
        it "returns Nothing if list contains antipodal points" $ do
            let points =
                    [ latLongDecimal 45 1
                    , latLongDecimal 45 2
                    , latLongDecimal 46 2
                    , latLongDecimal 46 1
                    , antipode (latLongDecimal 45 2)
                    ]
            mean points `shouldBe` Nothing
    describe "North pole" $
        it "returns (90, 0)" $ (northPole :: LatLong) `shouldBe` latLongDecimal 90.0 0.0
    describe "South pole" $
        it "returns (-90, 0)" $ (southPole :: LatLong) `shouldBe` latLongDecimal (-90.0) 0.0
