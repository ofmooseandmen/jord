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
    describe "alongTrackDistance" $ do
        it "returns a positive length when position is ahead start of great arc" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let g = minorArcBetween (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (alongTrackDistance p) g `shouldBe` Right (kilometres 62.3315757)
        it "returns a negative length when position is ahead start of great arc" $ do
            let p = s84Pos 53.3206 (-1.7297) zero
            let g = minorArcBetween (s84Pos 53.2611 (-0.7972) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (alongTrackDistance p) g `shouldBe` Right (kilometres (-62.3293209))
        it "returns 0 when position is start of great arc" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let g = minorArcBetween p (s84Pos 53.1887 0.1334 zero)
            fmap (alongTrackDistance p) g `shouldBe` Right zero
    describe "crossTrackDistance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let gc = greatCircleHeadingOn (s84Pos 53.3206 (-1.7297) zero) (decimalDegrees 96.0)
            crossTrackDistance p gc `shouldBe` metres (-305.6629)
        it "returns a negative length when position is left of great circle" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let gc = greatCircleThrough (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (crossTrackDistance p) gc `shouldBe` Right (metres (-307.5471))
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = s84Pos 53.261111 (-1.797222) zero
            let gc =
                    greatCircleHeadingOn
                        (s84Pos 53.320556 (-1.729722) zero)
                        (decimalDegrees 96.02166667)
            crossTrackDistance p gc `shouldBe` metres 7042.3859
        it "returns a positive length when position is left of great circle" $ do
            let p = antipode (s84Pos 53.2611 (-0.7972) zero)
            let gc = greatCircleThrough (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (crossTrackDistance p) gc `shouldBe` Right (metres 307.5471)
    describe "greatCircle through position" $
        it "fails if both positions are equal" $
        greatCircleThrough (s84Pos 3 154 zero) (s84Pos 3 154 zero) `shouldBe`
        Left "Invalid Great Circle: positions are equal"
    describe "interpolate" $ do
        let p1 = s84Pos 44 44 zero
        let p2 = s84Pos 46 46 zero
        it "fails if f < 0.0" $
            evaluate (interpolate p1 p2 (-0.5)) `shouldThrow`
            errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (interpolate p1 p2 1.1) `shouldThrow`
            errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $ interpolate p1 p2 0.0 `shouldBe` p1
        it "returns p1 if f == 1" $ interpolate p1 p2 1.0 `shouldBe` p2
        it "returns the interpolated position" $ do
            let p3 = s84Pos 53.479444 (-2.245278) (metres 10000)
            let p4 = s84Pos 55.605833 13.035833 (metres 20000)
            interpolate p3 p4 0.5 `shouldBe` s84Pos 54.7835569 5.1949852 (metres 15000)
    describe "isInsideSurface" $ do
        let p1 = s84Pos 45 1 zero
        let p2 = s84Pos 45 2 zero
        let p3 = s84Pos 46 1 zero
        let p4 = s84Pos 46 2 zero
        let p5 = s84Pos 45.1 1.1 zero
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
            let malmo = s84Pos 55.6050 13.0038 zero
            let ystad = s84Pos 55.4295 13.82 zero
            let lund = s84Pos 55.7047 13.1910 zero
            let helsingborg = s84Pos 56.0465 12.6945 zero
            let kristianstad = s84Pos 56.0294 14.1567 zero
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            let hoor = s84Pos 55.9295 13.5297 zero
            let hassleholm = s84Pos 56.1589 13.7668 zero
            isInsideSurface hoor polygon `shouldBe` True
            isInsideSurface hassleholm polygon `shouldBe` False
    describe "intersection" $ do
        it "returns nothing if both great arc are equals" $
            case minorArcBetween (s84Pos 51.885 0.235 zero) (s84Pos 52.885 1.235 zero) of
                Left e -> error e
                Right a -> intersection a a `shouldBe` Nothing
        it "returns nothing if both great arc are equals (opposite orientation)" $
            case minorArcBetween (s84Pos 51.885 0.235 zero) (s84Pos 52.885 1.235 zero) of
                Left e1 -> error e1
                Right a1 ->
                    case minorArcBetween (s84Pos 52.885 1.235 zero) (s84Pos 51.885 0.235 zero) of
                        Left e2 -> error e2
                        Right a2 -> intersection a1 a2 `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside either great arc" $
            case minorArcBetween (s84Pos 0 0 zero) (s84Pos 0 10 zero) of
                Left e1 -> error e1
                Right a1 ->
                    case minorArcBetween (s84Pos (-5) 5 zero) (s84Pos (-1) 5 zero) of
                        Left e2 -> error e2
                        Right a2 -> intersection a1 a2 `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside both great arcs" $
            case minorArcBetween (s84Pos 0 (-10) zero) (s84Pos 0 (-1) zero) of
                Left e1 -> error e1
                Right a1 ->
                    case minorArcBetween (s84Pos (-5) 5 zero) (s84Pos (-1) 5 zero) of
                        Left e2 -> error e2
                        Right a2 -> intersection a1 a2 `shouldBe` Nothing
        it "returns the point where the two great arcs intersect" $ do
            let spd = kilometresPerHour 1000
            let oneHour = hours 1
            let p11 = s84Pos 51.885 0.235 zero
            let p12 = positionAfter p11 (decimalDegrees 108.63) spd oneHour
            let p21 = s84Pos 49.008 2.549 zero
            let p22 = positionAfter p21 (decimalDegrees 32.72) spd oneHour
            case minorArcBetween p11 p12 of
                Left e1 -> error e1
                Right a1 ->
                    case minorArcBetween p21 p22 of
                        Left e2 -> error e2
                        Right a2 ->
                            intersection a1 a2 `shouldBe`
                            Just (s84Pos 50.9017225 4.494278333333333 zero)
    describe "intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc = greatCircleHeadingOn (s84Pos 51.885 0.235 zero) (decimalDegrees 108.63)
            intersections gc gc `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 =
                    fromRight
                        (error "err")
                        (greatCircleThrough (s84Pos 51.885 0.235 zero) (s84Pos 52.885 1.235 zero))
            let gc2 =
                    fromRight
                        (error "err")
                        (greatCircleThrough (s84Pos 52.885 1.235 zero) (s84Pos 51.885 0.235 zero))
            intersections gc1 gc2 `shouldBe` Nothing
        it "returns the two positions where the two great circles intersects" $ do
            let gc1 = greatCircleHeadingOn (s84Pos 51.885 0.235 zero) (decimalDegrees 108.63)
            let gc2 = greatCircleHeadingOn (s84Pos 49.008 2.549 zero) (decimalDegrees 32.72)
            let (i1, i2) = fromJust (intersections gc1 gc2)
            i1 `shouldBe` s84Pos 50.9017226 4.4942782 zero
            i2 `shouldBe` antipode i1
    describe "mean" $ do
        it "returns Nothing if no position is given" $
            (mean [] :: (Maybe (Position S84))) `shouldBe` Nothing
        it "returns the unique given position" $ do
            let p = s84Pos 50.066389 (-5.714722) zero
            mean [p] `shouldBe` Just p
        it "returns the geographical mean" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 15000.0)
            let p2 = s84Pos 58.643889 (-3.07) (metres 25000.0)
            let e = s84Pos 54.3622869 (-4.5306725) zero
            mean [p1, p2] `shouldBe` Just e
        it "returns Nothing if list contains antipodal positions" $ do
            let points =
                    [ s84Pos 45 1 zero
                    , s84Pos 45 2 zero
                    , s84Pos 46 2 zero
                    , s84Pos 46 1 zero
                    , antipode (s84Pos 45 2 zero)
                    ]
            mean points `shouldBe` Nothing