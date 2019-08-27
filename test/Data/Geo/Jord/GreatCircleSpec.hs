module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Control.Exception.Base (evaluate)
import Data.Either (fromRight)
import Data.Maybe (fromJust)

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "crossTrackDistance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = decimalLatLongPos 53.2611 (-0.7972) S84
            let gc =
                    greatCircleHeadingOn
                        (decimalLatLongPos 53.3206 (-1.7297) S84)
                        (decimalDegrees 96.0)
            crossTrackDistance p gc `shouldBe` metres (-305.6629)
        it "returns a negative length when position is left of great circle" $ do
            let p = decimalLatLongPos 53.2611 (-0.7972) S84
            let gc =
                    greatCircleThrough
                        (decimalLatLongPos 53.3206 (-1.7297) S84)
                        (decimalLatLongPos 53.1887 0.1334 S84)
            fmap (crossTrackDistance p) gc `shouldBe` Right (metres (-307.5471))
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = decimalLatLongPos 53.261111 (-1.797222) S84
            let gc =
                    greatCircleHeadingOn
                        (decimalLatLongPos 53.320556 (-1.729722) S84)
                        (decimalDegrees 96.02166667)
            crossTrackDistance p gc `shouldBe` metres 7042.3859
        it "returns a positive length when position is left of great circle" $ do
            let p = antipode (decimalLatLongPos 53.2611 (-0.7972) S84)
            let gc =
                    greatCircleThrough
                        (decimalLatLongPos 53.3206 (-1.7297) S84)
                        (decimalLatLongPos 53.1887 0.1334 S84)
            fmap (crossTrackDistance p) gc `shouldBe` Right (metres 307.5471)
    describe "greatCircle through position" $ do
        it "fails if both positions are equal" $
            greatCircleThrough (decimalLatLongPos 3 154 S84) (decimalLatLongPos 3 154 S84) `shouldBe`
            Left "Invalid Great Circle: positions are equal"
        it "fails if both positions are antipodal" $
            greatCircleThrough (decimalLatLongPos 3 154 S84) (antipode (decimalLatLongPos 3 154 S84)) `shouldBe`
            Left "Invalid Great Circle: positions are antipodal"
    describe "interpolate" $ do
        let p1 = decimalLatLongPos 44 44 S84
        let p2 = decimalLatLongPos 46 46 S84
        it "fails if f < 0.0" $
            evaluate (interpolate p1 p2 (-0.5)) `shouldThrow`
            errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (interpolate p1 p2 1.1) `shouldThrow`
            errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $ interpolate p1 p2 0.0 `shouldBe` p1
        it "returns p1 if f == 1" $ interpolate p1 p2 1.0 `shouldBe` p2
        it "returns the interpolated position" $ do
            let p3 = decimalLatLongHeightPos 53.479444 (-2.245278) (metres 10000) S84
            let p4 = decimalLatLongHeightPos 55.605833 13.035833 (metres 20000) S84
            interpolate p3 p4 0.5 `shouldBe`
                decimalLatLongHeightPos 54.7835569 5.1949852 (metres 15000) S84
    describe "isInsideSurface" $ do
        let p1 = decimalLatLongPos 45 1 S84
        let p2 = decimalLatLongPos 45 2 S84
        let p3 = decimalLatLongPos 46 1 S84
        let p4 = decimalLatLongPos 46 2 S84
        let p5 = decimalLatLongPos 45.1 1.1 S84
        it "return False if polygon is empty" $ isInsideSurface p1 [] `shouldBe` False
        it "return False if polygon does not define at least a triangle" $
            isInsideSurface p1 [p1, p2] `shouldBe` False
        it "returns True if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            isInsideSurface p5 polygon `shouldBe` True
        it "returns False if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            let p = antipode p5
            isInsideSurface p polygon `shouldBe` False
        it "returns False if position is a vertex of the polygon" $ do
            let polygon = [p1, p2, p4, p3]
            isInsideSurface p1 polygon `shouldBe` False
        it "handles closed polygons" $ do
            let polygon = [p1, p2, p4, p3, p1]
            isInsideSurface p5 polygon `shouldBe` True
        it "handles concave polygons" $ do
            let malmo = decimalLatLongPos 55.6050 13.0038 S84
            let ystad = decimalLatLongPos 55.4295 13.82 S84
            let lund = decimalLatLongPos 55.7047 13.1910 S84
            let helsingborg = decimalLatLongPos 56.0465 12.6945 S84
            let kristianstad = decimalLatLongPos 56.0294 14.1567 S84
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            let hoor = decimalLatLongPos 55.9295 13.5297 S84
            let hassleholm = decimalLatLongPos 56.1589 13.7668 S84
            isInsideSurface hoor polygon `shouldBe` True
            isInsideSurface hassleholm polygon `shouldBe` False
    describe "intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc = greatCircleHeadingOn (decimalLatLongPos 51.885 0.235 S84) (decimalDegrees 108.63)
            intersections gc gc `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 =
                    fromRight
                        (error "err")
                        (greatCircleThrough
                             (decimalLatLongPos 51.885 0.235 S84)
                             (decimalLatLongPos 52.885 1.235 S84))
            let gc2 =
                    fromRight
                        (error "err")
                        (greatCircleThrough
                             (decimalLatLongPos 52.885 1.235 S84)
                             (decimalLatLongPos 51.885 0.235 S84))
            intersections gc1 gc2 `shouldBe` Nothing
        it "returns the two positions where the two great circles intersects" $ do
            let gc1 = greatCircleHeadingOn (decimalLatLongPos 51.885 0.235 S84) (decimalDegrees 108.63)
            let gc2 = greatCircleHeadingOn (decimalLatLongPos 49.008 2.549 S84) (decimalDegrees 32.72)
            let (i1, i2) = fromJust (intersections gc1 gc2)
            i1 `shouldBe` decimalLatLongPos 50.9017226 4.4942782 S84
            i2 `shouldBe` antipode i1
    describe "mean" $ do
        it "returns Nothing if no position is given" $
            (mean [] :: (Maybe (Position S84))) `shouldBe` Nothing
        it "returns the unique given position" $ do
            let p = decimalLatLongPos 50.066389 (-5.714722) S84
            mean [p] `shouldBe` Just p
        it "returns the geographical mean" $ do
            let p1 = decimalLatLongHeightPos 50.066389 (-5.714722) (metres 15000.0) S84
            let p2 = decimalLatLongHeightPos 58.643889 (-3.07) (metres 25000.0) S84
            let e = decimalLatLongHeightPos 54.3622869 (-4.5306725) zero S84
            mean [p1, p2] `shouldBe` Just e
        it "returns Nothing if list contains antipodal positions" $ do
            let points =
                    [ decimalLatLongPos 45 1 S84
                    , decimalLatLongPos 45 2 S84
                    , decimalLatLongPos 46 2 S84
                    , decimalLatLongPos 46 1 S84
                    , antipode (decimalLatLongPos 45 2 S84)
                    ]
            mean points `shouldBe` Nothing