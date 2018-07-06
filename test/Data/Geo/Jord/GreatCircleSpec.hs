module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Control.Exception.Base
import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Data.Maybe
import Test.Hspec

spec :: Spec
spec = do
    describe "antipode" $ do
        it "returns the antipodal point" $
            antipode (readGeoPos "484137N0061105E") `geoShouldBe`
            geoPos (-48.6936111) (-173.8152777)
        it "returns the south pole when called with the north pole" $
            antipode (northPole :: GeoPos) `shouldBe` geoPos (-90.0) (-180.0)
        it "returns the north pole when called with the south pole" $
            antipode (southPole :: GeoPos) `shouldBe` geoPos 90.0 (-180.0)
    describe "distance" $ do
        it "returns 0 if both points are equal" $
            distance (readGeoPos "500359N1795959W") (readGeoPos "500359N1795959W") `lengthShouldBe`
            ofMetres 0.0
        it "returns the distance between 2 points" $
            distance (readGeoPos "500359N0054253W") (readGeoPos "583838N0030412W") `lengthShouldBe`
            ofMetres 968854.8849506
        it "handles singularity at the pole" $
            distance (northPole :: GeoPos) (southPole :: GeoPos) `lengthShouldBe`
            ofMetres 2.00151144420359e7
        it "handles the discontinuity at the Date Line" $
            distance (readGeoPos "500359N1795959W") (readGeoPos "500359N1795959E") `lengthShouldBe`
            ofMetres 39.6533738
    describe "destination" $ do
        it "return the given point if distance is 0 meter" $
            destination (readGeoPos "531914N0014347W") (ofDegrees 96.0217) (ofMetres 0) `shouldBe`
            readGeoPos "531914N0014347W"
        it "return the destination point along great-circle at distance and bearing" $
            destination (readGeoPos "531914N0014347W") (ofDegrees 96.0217) (ofMetres 124800) `geoShouldBe`
            geoPos 53.1882691 0.1332742
    describe "initialBearing" $ do
        it "returns the 0 if both point are the same" $
            initialBearing (readGeoPos "500359N0054253W") (readGeoPos "500359N0054253W") `shouldBe`
            ofDegrees 0
        it "returns the initial bearing in compass degrees" $
            initialBearing (readGeoPos "500359N0054253W") (readGeoPos "583838N0030412W") `angleShouldBe`
            ofDegrees 9.1198181
        it "returns the initial bearing in compass degrees" $
            initialBearing (readGeoPos "583838N0030412W") (readGeoPos "500359N0054253W") `angleShouldBe`
            ofDegrees 191.2752012
    describe "interpolate" $ do
        it "fails if t0 > t1" $
            evaluate
                (interpolate
                     (readGeoPos "44N044E")
                     (Millis 1)
                     (readGeoPos "46N046E")
                     (Millis 0)
                     (Millis 2)) `shouldThrow`
            errorCall "expected t0 <= ti <= t1"
        it "fails if ti < t0" $
            evaluate
                (interpolate
                     (readGeoPos "44N044E")
                     (Millis 1)
                     (readGeoPos "46N046E")
                     (Millis 2)
                     (Millis 0)) `shouldThrow`
            errorCall "expected t0 <= ti <= t1"
        it "fails if ti > t1" $
            evaluate
                (interpolate
                     (readGeoPos "44N044E")
                     (Millis 1)
                     (readGeoPos "46N046E")
                     (Millis 2)
                     (Millis 3)) `shouldThrow`
            errorCall "expected t0 <= ti <= t1"
        it "returns p0 if ti == t0" $
            interpolate
                (readGeoPos "44N044E")
                (Millis 1)
                (readGeoPos "46N046E")
                (Millis 2)
                (Millis 1) `shouldBe`
            readGeoPos "44N044E"
        it "returns p1 if ti == t1" $
            interpolate
                (readGeoPos "44N044E")
                (Millis 1)
                (readGeoPos "46N046E")
                (Millis 2)
                (Millis 2) `shouldBe`
            readGeoPos "46N046E"
        it "returns the interpolated position" $
            interpolate
                (readGeoPos "53°28'46''N 2°14'43''W")
                (Millis 0)
                (readGeoPos "55°36'21''N 13°02'09''E")
                (Millis 100)
                (Millis 50) `geoShouldBe`
            geoPos 54.7835574 5.1949856
    describe "intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc = greatCircleBearing (geoPos 51.885 0.235) (ofDegrees 108.63)
            (intersections gc gc :: Maybe (GeoPos, GeoPos)) `shouldBe` Nothing
        it "returns the two points where the two great circles intersects" $ do
            let gc1 = greatCircleBearing (geoPos 51.885 0.235) (ofDegrees 108.63)
            let gc2 = greatCircleBearing (geoPos 49.008 2.549) (ofDegrees 32.72)
            let (i1, i2) = fromJust (intersections gc1 gc2)
            i1 `geoShouldBe` geoPos 50.9017226 4.4942782
            i2 `geoShouldBe` antipode i1
    describe "finalBearing" $ do
        it "returns the 180.0 if both point are the same" $
            finalBearing (readGeoPos "500359N0054253W") (readGeoPos "500359N0054253W") `shouldBe`
            ofDegrees 180
        it "returns the final bearing in compass degrees" $
            finalBearing (readGeoPos "500359N0054253W") (readGeoPos "583838N0030412W") `angleShouldBe`
            ofDegrees 11.2752012
        it "returns the final bearing in compass degrees" $
            finalBearing (readGeoPos "583838N0030412W") (readGeoPos "500359N0054253W") `angleShouldBe`
            ofDegrees 189.1198181
        it "returns the final bearing in compass degrees" $
            finalBearing (readGeoPos "535941S0255915W") (readGeoPos "54N154E") `angleShouldBe`
            ofDegrees 125.6839436
    describe "midpoint" $ do
        it "fails if no point is given" $
            evaluate (midpoint [] :: GeoPos) `shouldThrow`
            errorCall "midpoint expects a non-empty list"
        it "returns the unique given point" $
            midpoint [readGeoPos "500359N0054253W"] `shouldBe` readGeoPos "500359N0054253W"
        it "returns the mid point between given points" $
            midpoint [readGeoPos "500359N0054253W", readGeoPos "583838N0030412W"] `geoShouldBe`
            geoPos 54.3622868 (-4.5306725)
    describe "north pole" $ it "returns (90, 0)" $ (northPole :: GeoPos) `shouldBe` geoPos 90.0 0.0
    describe "south pole" $
        it "returns (-90, 0)" $ (southPole :: GeoPos) `shouldBe` geoPos (-90.0) 0.0
