module Data.Geo.Jord.GeodeticsSpec
    ( spec
    ) where

import Control.Exception.Base
import Data.Geo.Jord
import Data.Maybe (fromJust)
import Test.Hspec

spec :: Spec
spec = do
    describe "alongTrackDistance" $ do
        it "returns a positive length when position is ahead start of great arc" $ do
            let p = decimalLatLong 53.2611 (-0.7972)
            let ga = greatArc (decimalLatLong 53.3206 (-1.7297), decimalLatLong 53.1887 0.1334)
            alongTrackDistance p ga r84 `shouldBe` kilometres 62.3315757
        it "returns a negative length when position is ahead start of great arc" $ do
            let p = decimalLatLong 53.3206 (-1.7297)
            let ga = greatArc (decimalLatLong 53.2611 (-0.7972), decimalLatLong 53.1887 0.1334)
            alongTrackDistance p ga r84 `shouldBe` kilometres (-62.3293209)
        it "returns a 0 when position is start of great arc" $ do
            let p = decimalLatLong 53.2611 (-0.7972)
            let ga = greatArc (p, decimalLatLong 53.1887 0.1334)
            alongTrackDistance p ga r84 `shouldBe` zero
    describe "antipode" $ do
        it "returns the antipodal point" $ do
            let p = latLongHeight (readLatLong "484137N0061105E") (metres 15000)
            let e = decimalLatLongHeight (-48.6936111) (-173.8152777) (metres 15000)
            antipode p `shouldBe` e
        it "returns the south pole when called with the north pole" $
            antipode northPole `shouldBe` southPole
        it "returns the north pole when called with the south pole" $
            antipode southPole `shouldBe` northPole
    describe "crossTrackDistance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = decimalLatLong 53.2611 (-0.7972)
            let gc = greatCircle (decimalLatLong 53.3206 (-1.7297), decimalDegrees 96.0)
            crossTrackDistance p gc r84 `shouldBe` metres (-305.6629)
        it "returns a negative length when position is left of great circle" $ do
            let p = decimalLatLong 53.2611 (-0.7972)
            let gc = greatCircle (decimalLatLong 53.3206 (-1.7297), decimalLatLong 53.1887 0.1334)
            crossTrackDistance p gc r84 `shouldBe` metres (-307.5471)
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = readLatLong "531540N0014750W"
            let gc = greatCircle (readLatLong "531914N0014347W", readAngle "96d01m18s")
            crossTrackDistance p gc r84 `shouldBe` metres 7042.3242
        it "returns a positive length when position is left of great circle" $ do
            let p = antipode (decimalLatLong 53.2611 (-0.7972))
            let gc = greatCircle (decimalLatLong 53.3206 (-1.7297), decimalLatLong 53.1887 0.1334)
            crossTrackDistance p gc r84 `shouldBe` metres 307.5471
    describe "destination" $ do
        it "return the given point if distance is 0 meter" $ do
            let p0 = readLatLong "531914N0014347W"
            destination p0 (decimalDegrees 96.0217) zero r84 `shouldBe` p0
        it "return the angular position along great-circle at distance and bearing" $ do
            let p0 = latLongHeight (readLatLong "531914N0014347W") (metres 15000.0)
            let p1 = decimalLatLongHeight 53.1882691 0.1332744 (metres 15000.0)
            destination p0 (decimalDegrees 96.0217) (metres 124800) r84 `shouldBe` p1
        it "return the ECEF position along great-circle at distance and bearing" $ do
            let p0 = ecefToNVector (ecefMetres 3812864.094 (-115142.863) 5121515.161) s84
            let p1 = ecefMetres 3826406.471 8900.5364 5112694.2331
            let p = destination84 p0 (decimalDegrees 96.0217) (metres 124800)
            nvectorToEcef p s84 `shouldBe` p1
    describe "finalBearing" $ do
        it "returns the Nothing if both point are the same" $ do
            let p = readLatLong "500359N0054253W"
            finalBearing p p `shouldBe` Nothing
        it "returns 0° if both point have the same longitude (going north)" $ do
            let p1 = latLongHeight (readLatLong "500359N0054253W") (metres 12000)
            let p2 = latLongHeight (readLatLong "583838N0054253W") (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° if both point have the same longitude (going south)" $ do
            let p1 = latLongHeight (readLatLong "583838N0054253W") (metres 12000)
            let p2 = latLongHeight (readLatLong "500359N0054253W") (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = latLongHeight (readLatLong "000000N0000000E") (metres 12000)
            let p2 = latLongHeight (readLatLong "000000N0010000E") (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = latLongHeight (readLatLong "000000N0010000E") (metres 12000)
            let p2 = latLongHeight (readLatLong "000000N0000000E") (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 270)
        it "returns the final bearing in compass angle" $ do
            let p1 = readLatLong "500359N0054253W"
            let p2 = readLatLong "583838N0030412W"
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 11.2752013)
        it "returns the final bearing in compass angle" $ do
            let p1 = readLatLong "583838N0030412W"
            let p2 = readLatLong "500359N0054253W"
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 189.1198181)
        it "returns the final bearing in compass angle" $ do
            let p1 = readLatLong "535941S0255915W"
            let p2 = readLatLong "54N154E"
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 125.6839436)
    describe "greatCircle smart constructors" $ do
        it "fails if both positions are equal" $
            greatCircleE (decimalLatLong 3 154, decimalLatLong 3 154) `shouldBe`
            Left "Invalid Great Circle: positions are equal"
        it "fails if both positions are antipodal" $
            greatCircleE (decimalLatLong 3 154, antipode (decimalLatLong 3 154)) `shouldBe`
            Left "Invalid Great Circle: positions are antipodal"
    describe "initialBearing" $ do
        it "returns Nothing if both point are the same" $ do
            let p = readLatLong "500359N1795959W"
            initialBearing p p `shouldBe` Nothing
        it "returns 0° if both point have the same longitude (going north)" $ do
            let p1 = latLongHeight (readLatLong "500359N0054253W") (metres 12000)
            let p2 = latLongHeight (readLatLong "583838N0054253W") (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° if both point have the same longitude (going south)" $ do
            let p1 = latLongHeight (readLatLong "583838N0054253W") (metres 12000)
            let p2 = latLongHeight (readLatLong "500359N0054253W") (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = latLongHeight (readLatLong "000000N0000000E") (metres 12000)
            let p2 = latLongHeight (readLatLong "000000N0010000E") (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = latLongHeight (readLatLong "000000N0010000E") (metres 12000)
            let p2 = latLongHeight (readLatLong "000000N0000000E") (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 270)
        it "returns the initial bearing in compass angle" $ do
            let p1 = latLongHeight (readLatLong "500359N0054253W") (metres 12000)
            let p2 = latLongHeight (readLatLong "583838N0030412W") (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 9.1198181)
        it "returns the initial bearing in compass angle" $ do
            let p1 = latLongHeight (readLatLong "583838N0030412W") (metres 12000)
            let p2 = latLongHeight (readLatLong "500359N0054253W") (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 191.2752013)
    describe "interpolate" $ do
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
            let p3 = latLongHeight (readLatLong "53°28'46''N 2°14'43''W") (metres 10000)
            let p4 = latLongHeight (readLatLong "55°36'21''N 13°02'09''E") (metres 20000)
            interpolate p3 p4 0.5 `shouldBe`
                decimalLatLongHeight 54.7835574 5.1949856 (metres 15000)
    describe "intersection" $ do
        it "returns nothing if both great arc are equals" $ do
            let ga = greatArc (decimalLatLong 51.885 0.235, decimalLatLong 52.885 1.235)
            (intersection ga ga :: Maybe LatLong) `shouldBe` Nothing
        it "returns nothing if both great arc are equals (opposite orientation)" $ do
            let ga1 = greatArc (decimalLatLong 51.885 0.235, decimalLatLong 52.885 1.235)
            let ga2 = greatArc (decimalLatLong 51.885 0.235, decimalLatLong 52.885 1.235)
            (intersection ga1 ga2 :: Maybe LatLong) `shouldBe` Nothing
        it "returns the point where the two great arcs intersect" $ do
            let spd = kilometresPerHour 1000
            let t1 = Track (decimalLatLong 51.885 0.235) (decimalDegrees 108.63) spd
            let t2 = Track (decimalLatLong 49.008 2.549) (decimalDegrees 32.72) spd
            let oneHour = hours 1
            let ga1 = greatArc (t1, oneHour)
            let ga2 = greatArc (t2, oneHour)
            (intersection ga1 ga2 :: Maybe LatLong) `shouldBe`
                Just (decimalLatLong 50.9017225 4.494278333333333)
    describe "intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc = greatCircle (decimalLatLong 51.885 0.235, decimalDegrees 108.63)
            (intersections gc gc :: Maybe (LatLong, LatLong)) `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 = greatCircle (decimalLatLong 51.885 0.235, decimalLatLong 52.885 1.235)
            let gc2 = greatCircle (decimalLatLong 52.885 1.235, decimalLatLong 51.885 0.235)
            (intersections gc1 gc2 :: Maybe (LatLong, LatLong)) `shouldBe` Nothing
        it "returns the two points where the two great circles intersect" $ do
            let gc1 = greatCircle (decimalLatLong 51.885 0.235, decimalDegrees 108.63)
            let gc2 = greatCircle (decimalLatLong 49.008 2.549, decimalDegrees 32.72)
            let (i1, i2) = fromJust (intersections gc1 gc2)
            i1 `shouldBe` decimalLatLong 50.9017226 4.4942782
            i2 `shouldBe` antipode i1
    describe "isInsideSurface" $ do
        let p1 = decimalLatLong 45 1
        let p2 = decimalLatLong 45 2
        let p3 = decimalLatLong 46 1
        let p4 = decimalLatLong 46 2
        let p5 = decimalLatLong 45.1 1.1
        it "return False if polygon is empty" $ isInsideSurface p1 [] `shouldBe` False
        it "return False if polygon does not define at least a triangle" $
            isInsideSurface p1 [p1, p2] `shouldBe` False
        it "returns True if point is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            isInsideSurface p5 polygon `shouldBe` True
        it "returns False if point is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            let p = antipode p5
            isInsideSurface p polygon `shouldBe` False
        it "returns False if point is a vertex of the polygon" $ do
            let polygon = [p1, p2, p4, p3]
            isInsideSurface p1 polygon `shouldBe` False
        it "handles closed polygons" $ do
            let polygon = [p1, p2, p4, p3, p1]
            isInsideSurface p5 polygon `shouldBe` True
        it "handles concave polygons" $ do
            let malmo = decimalLatLong 55.6050 13.0038
            let ystad = decimalLatLong 55.4295 13.82
            let lund = decimalLatLong 55.7047 13.1910
            let helsingborg = decimalLatLong 56.0465 12.6945
            let kristianstad = decimalLatLong 56.0294 14.1567
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            let hoor = decimalLatLong 55.9295 13.5297
            let hassleholm = decimalLatLong 56.1589 13.7668
            isInsideSurface hoor polygon `shouldBe` True
            isInsideSurface hassleholm polygon `shouldBe` False
    describe "mean" $ do
        it "returns Nothing if no point is given" $ (mean [] :: Maybe NVector) `shouldBe` Nothing
        it "returns the unique given point" $ do
            let p = readLatLong "500359N0054253W"
            mean [p] `shouldBe` Just p
        it "returns the geographical mean" $ do
            let p1 = latLongHeight (readLatLong "500359N0054253W") (metres 15000.0)
            let p2 = latLongHeight (readLatLong "583838N0030412W") (metres 25000.0)
            let e = decimalLatLongHeight 54.3622869 (-4.5306725) zero
            mean [p1, p2] `shouldBe` Just e
        it "returns Nothing if list contains antipodal points" $ do
            let points =
                    [ decimalLatLong 45 1
                    , decimalLatLong 45 2
                    , decimalLatLong 46 2
                    , decimalLatLong 46 1
                    , antipode (decimalLatLong 45 2)
                    ]
            mean points `shouldBe` Nothing
    describe "surfaceDistance" $ do
        it "returns 0 if both points are equal" $ do
            let p = readLatLong "500359N1795959W"
            surfaceDistance p p r84 `shouldBe` zero
        it "returns the distance between 2 points" $ do
            let p1 = readLatLong "500359N0054253W"
            let p2 = readLatLong "583838N0030412W"
            surfaceDistance84 p1 p2 `shouldBe` metres 968854.8685
        it "handles singularity at the pole" $
            surfaceDistance northPole southPole r84 `shouldBe` kilometres 20015.114352200002
        it "handles the discontinuity at the Date Line" $ do
            let p1 = readLatLong "500359N1795959W"
            let p2 = readLatLong "500359N1795959E"
            surfaceDistance p1 p2 (meanRadius wgs84) `shouldBe` metres 39.6596
