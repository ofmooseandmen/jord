module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Control.Exception.Base
import Data.Geo.Jord
import Data.Maybe
import Test.Hspec

spec :: Spec
spec = do
    describe "Antipode" $ do
        it "returns the antipodal point" $
            antipode (readGeoPos "484137N0061105E") `shouldBe`
            latLongDecimal (-48.6936111) (-173.8152777)
        it "returns the south pole when called with the north pole" $
            antipode (northPole :: GeoPos) `shouldBe` latLongDecimal (-90.0) (-180.0)
        it "returns the north pole when called with the south pole" $
            antipode (southPole :: GeoPos) `shouldBe` latLongDecimal 90.0 (-180.0)
    describe "Cross Track Distance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = latLongDecimal 53.2611 (-0.7972)
            let gc = greatCircleBearing (latLongDecimal 53.3206 (-1.7297)) (decimalDegrees 96.0)
            crossTrackDistance p gc `shouldBe` metres (-305.663)
        it "returns a negative length when position is left of great circle" $ do
            let p = latLongDecimal 53.2611 (-0.7972)
            let gc = greatCircle (latLongDecimal 53.3206 (-1.7297)) (latLongDecimal 53.1887 0.1334)
            crossTrackDistance p gc `shouldBe` metres (-307.547)
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = readGeoPos "531540N0014750W"
            let gc = greatCircleBearing (readGeoPos "531914N0014347W") (readAngle "96d01m18s")
            crossTrackDistance p gc `shouldBe` metres 7042.324
    describe "Distance" $ do
        it "returns 0 if both points are equal" $
            distance (readGeoPos "500359N1795959W") (readGeoPos "500359N1795959W") `shouldBe`
            metres 0.0
        it "returns the distance between 2 points" $
            distance (readGeoPos "500359N0054253W") (readGeoPos "583838N0030412W") `shouldBe`
            metres 968854.873
        it "handles singularity at the pole" $
            distance (northPole :: GeoPos) (southPole :: GeoPos) `shouldBe`
            metres 2.00151144420359e7
        it "handles the discontinuity at the Date Line" $
            distance (readGeoPos "500359N1795959W") (readGeoPos "500359N1795959E") `shouldBe`
            metres 39.66
    describe "Destination" $ do
        it "return the given point if distance is 0 meter" $
            destination (readGeoPos "531914N0014347W") (decimalDegrees 96.0217) (metres 0) `shouldBe`
            readGeoPos "531914N0014347W"
        it "return the destination point along great-circle at distance and bearing" $
            destination (readGeoPos "531914N0014347W") (decimalDegrees 96.0217) (metres 124800) `shouldBe`
            latLongDecimal 53.1882691 0.1332744
    describe "Initial bearing" $ do
        it "returns the 0 if both point are the same" $
            initialBearing (readGeoPos "500359N0054253W") (readGeoPos "500359N0054253W") `shouldBe`
            decimalDegrees 0
        it "returns the initial bearing in compass degrees" $
            initialBearing (readGeoPos "500359N0054253W") (readGeoPos "583838N0030412W") `shouldBe`
            decimalDegrees 9.1198181
        it "returns the initial bearing in compass degrees" $
            initialBearing (readGeoPos "583838N0030412W") (readGeoPos "500359N0054253W") `shouldBe`
            decimalDegrees 191.2752013
    describe "Interpolate" $ do
        it "fails if f < 0.0" $
            evaluate (interpolate (readGeoPos "44N044E") (readGeoPos "46N046E") (-0.5)) `shouldThrow`
            errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (interpolate (readGeoPos "44N044E") (readGeoPos "46N046E") 1.1) `shouldThrow`
            errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $
            interpolate (readGeoPos "44N044E") (readGeoPos "46N046E") 0.0 `shouldBe`
            readGeoPos "44N044E"
        it "returns p1 if f == 1" $
            interpolate (readGeoPos "44N044E") (readGeoPos "46N046E") 1.0 `shouldBe`
            readGeoPos "46N046E"
        it "returns the interpolated position" $
            interpolate
                (readGeoPos "53°28'46''N 2°14'43''W")
                (readGeoPos "55°36'21''N 13°02'09''E")
                0.5 `shouldBe`
            latLongDecimal 54.7835574 5.1949856
    describe "Intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc = greatCircleBearing (latLongDecimal 51.885 0.235) (decimalDegrees 108.63)
            (intersections gc gc :: Maybe (GeoPos, GeoPos)) `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 = greatCircle (latLongDecimal 51.885 0.235) (latLongDecimal 52.885 1.235)
            let gc2 = greatCircle (latLongDecimal 52.885 1.235) (latLongDecimal 51.885 0.235)
            (intersections gc1 gc2 :: Maybe (GeoPos, GeoPos)) `shouldBe` Nothing
        it "returns the two points where the two great circles intersects" $ do
            let gc1 = greatCircleBearing (latLongDecimal 51.885 0.235) (decimalDegrees 108.63)
            let gc2 = greatCircleBearing (latLongDecimal 49.008 2.549) (decimalDegrees 32.72)
            let (i1, i2) = fromJust (intersections gc1 gc2)
            i1 `shouldBe` latLongDecimal 50.9017226 4.4942782
            i2 `shouldBe` antipode i1
    describe "Final bearing" $ do
        it "returns the 180.0 if both point are the same" $
            finalBearing (readGeoPos "500359N0054253W") (readGeoPos "500359N0054253W") `shouldBe`
            decimalDegrees 180
        it "returns the final bearing in compass degrees" $
            finalBearing (readGeoPos "500359N0054253W") (readGeoPos "583838N0030412W") `shouldBe`
            decimalDegrees 11.2752013
        it "returns the final bearing in compass degrees" $
            finalBearing (readGeoPos "583838N0030412W") (readGeoPos "500359N0054253W") `shouldBe`
            decimalDegrees 189.1198181
        it "returns the final bearing in compass degrees" $
            finalBearing (readGeoPos "535941S0255915W") (readGeoPos "54N154E") `shouldBe`
            decimalDegrees 125.6839436
    describe "Great Circle Smart constructors" $ do
        it "fails if both positions are equal" $
            greatCircleE (latLongDecimal 3 154) (latLongDecimal 3 154) `shouldBe`
            Left "Invalid Great Circle: positions are equal"
        it "fails if both positions are antipodal" $
            greatCircleE (latLongDecimal 3 154) (antipode (latLongDecimal 3 154)) `shouldBe`
            Left "Invalid Great Circle: positions are antipodal"
    describe "Midpoint" $ do
        it "fails if no point is given" $
            evaluate (midpoint [] :: GeoPos) `shouldThrow`
            errorCall "midpoint expects a non-empty list"
        it "returns the unique given point" $
            midpoint [readGeoPos "500359N0054253W"] `shouldBe` readGeoPos "500359N0054253W"
        it "returns the mid point between given points" $
            midpoint [readGeoPos "500359N0054253W", readGeoPos "583838N0030412W"] `shouldBe`
            latLongDecimal 54.3622869 (-4.5306725)
    describe "North pole" $
        it "returns (90, 0)" $ (northPole :: GeoPos) `shouldBe` latLongDecimal 90.0 0.0
    describe "South pole" $
        it "returns (-90, 0)" $ (southPole :: GeoPos) `shouldBe` latLongDecimal (-90.0) 0.0
