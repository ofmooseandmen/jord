module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Control.Exception.Base (evaluate)
import Control.Monad (join)
import Data.Maybe (fromJust)

import Test.Hspec

import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.Position

spec :: Spec
spec = do
    describe "alongTrackDistance" $ do
        it "returns a positive length when position is ahead start of great arc" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let g = minorArc (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (alongTrackDistance p) g `shouldBe` Just (kilometres 62.3315791)
        it "returns a negative length when position is ahead start of great arc" $ do
            let p = s84Pos 53.3206 (-1.7297) zero
            let g = minorArc (s84Pos 53.2611 (-0.7972) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (alongTrackDistance p) g `shouldBe` Just (kilometres (-62.329309979))
        it "returns 0 when position is start of great arc" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let g = minorArc p (s84Pos 53.1887 0.1334 zero)
            fmap (alongTrackDistance p) g `shouldBe` Just zero
    describe "crossTrackDistance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let gc = greatCircleHeadingOn (s84Pos 53.3206 (-1.7297) zero) (decimalDegrees 96.0)
            crossTrackDistance p gc `shouldBe` metres (-305.665267)
        it "returns a negative length when position is left of great circle" $ do
            let p = s84Pos 53.2611 (-0.7972) zero
            let gc = greatCircleThrough (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (crossTrackDistance p) gc `shouldBe` Just (metres (-307.549992))
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = s84Pos 53.261111 (-1.797222) zero
            let gc = greatCircleHeadingOn (s84Pos 53.320556 (-1.729722) zero) (decimalDegrees 96.02166667)
            crossTrackDistance p gc `shouldBe` metres 7042.396068
        it "returns a positive length when position is left of great circle" $ do
            let p = antipode (s84Pos 53.2611 (-0.7972) zero)
            let gc = greatCircleThrough (s84Pos 53.3206 (-1.7297) zero) (s84Pos 53.1887 0.1334 zero)
            fmap (crossTrackDistance p) gc `shouldBe` Just (metres 307.549992)
        it "return zero when position is on the great circle" $ do
            let gc1 = s84Pos 53.3206 (-1.7297) zero
            let gc2 = s84Pos 53.1887 (0.1334) zero
            let gc = fromJust $ greatCircleThrough gc1 gc2
            let ps = fmap (\f -> interpolate gc1 gc2 f) (take 11 (iterate (\x -> x + 0.1 ::Double) 0.0))
            fmap (\p -> crossTrackDistance p gc) ps `shouldBe` (replicate 11 zero)
    describe "destination" $ do
        it "return the given position if distance is 0 meter" $ do
            let p0 = s84Pos 53.320556 (-1.729722) zero
            destination p0 (decimalDegrees 96.0217) zero `shouldBe` p0
        it "return the position along the great circle at distance and bearing" $ do
            let p0 = s84Pos 53.320556 (-1.729722) (metres 15000.0)
            let p1 = s84Pos 53.18826954833333 0.13327449055555557 (metres 15000.0)
            destination p0 (decimalDegrees 96.0217) (metres 124800) `shouldBe` p1
    describe "surfaceDistance" $ do
        it "returns 0 if both points are equal" $ do
            let p = s84Pos 50.066389 (-5.714722) (metres 15000.0)
            surfaceDistance p p `shouldBe` zero
        it "returns the distance between 2 points" $ do
            let p1 = s84Pos 50.066389 (-5.714722) zero
            let p2 = s84Pos 58.643889 (-3.07) zero
            surfaceDistance p1 p2 `shouldBe` metres 968854.878007
        it "handles singularity at the pole" $
            surfaceDistance (northPole S84) (southPole S84) `shouldBe` kilometres 20015.114352233
        it "handles the discontinuity at the Date Line" $ do
            let p1 = s84Pos 50.066389 (-179.999722) zero
            let p2 = s84Pos 50.066389 179.999722 zero
            surfaceDistance p1 p2 `shouldBe` metres 39.685092
    describe "greatCircle through position" $
        it "fails if both positions are equal" $
        greatCircleThrough (s84Pos 3 154 zero) (s84Pos 3 154 zero) `shouldBe` Nothing
    describe "finalBearing" $ do
        it "returns the Nothing if both positions are the same (ignoring height)" $ do
            let p = s84Pos 50.066389 (-5.714722) zero
            finalBearing p p `shouldBe` Nothing
            finalBearing p (s84Pos 50.066389 (-5.714722) (metres 10)) `shouldBe` Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 12000)
            let p2 = s84Pos 58.643889 (-5.714722) (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = s84Pos 58.643889 (-5.714722) (metres 5000)
            let p2 = s84Pos 50.066389 (-5.714722) (metres 12000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = s84Pos 0 0 (metres 12000)
            let p2 = s84Pos 0 1 (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = s84Pos 0 1 (metres 12000)
            let p2 = s84Pos 0 0 (metres 5000)
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 270)
        it "returns the final bearing in compass angle" $ do
            let p1 = s84Pos 50.066389 (-5.714722) zero
            let p2 = s84Pos 58.643889 (-3.07) zero
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 11.27520031611111)
        it "returns the final bearing in compass angle" $ do
            let p1 = s84Pos 58.643889 (-3.07) zero
            let p2 = s84Pos 50.066389 (-5.714722) zero
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 189.1198173275)
        it "returns the final bearing in compass angle" $ do
            let p1 = s84Pos (-53.994722) (-25.9875) zero
            let p2 = s84Pos 54 154 zero
            finalBearing p1 p2 `shouldBe` Just (decimalDegrees 125.68508662305555)
    describe "initialBearing" $ do
        it "returns Nothing if both positions are the same (ignoring height)" $ do
            let p = s84Pos 50.066389 (-179.999722) zero
            initialBearing p p `shouldBe` Nothing
            initialBearing p (s84Pos 50.066389 (-179.999722) (metres 100)) `shouldBe` Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 12000)
            let p2 = s84Pos 58.643889 (-5.714722) (metres 12000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = s84Pos 58.643889 (-5.714722) (metres 12000)
            let p2 = s84Pos 50.066389 (-5.714722) (metres 12000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = s84Pos 0 0 (metres 12000)
            let p2 = s84Pos 0 1 (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = s84Pos 0 1 (metres 12000)
            let p2 = s84Pos 0 0 (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 270)
        it "returns 0° at the prime meridian going north" $ do
            let p1 = s84Pos 50 0 zero
            let p2 = s84Pos 58 0 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° at the prime meridian going south" $ do
            let p1 = s84Pos 58 0 zero
            let p2 = s84Pos 50 0 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 0° at the date line going north" $ do
            let p1 = s84Pos 50 180 zero
            let p2 = s84Pos 58 180 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 180° at the date line going south" $ do
            let p1 = s84Pos 58 180 zero
            let p2 = s84Pos 50 180 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 180)
        it "returns 0° going from the south pole to the north pole" $ do
            let p1 = southPole S84
            let p2 = northPole S84
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 0° going from the north pole to the south pole" $ do
            let p1 = northPole S84
            let p2 = southPole S84
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns 0° going from the south pole to anywhere on the date line" $ do
            let p1 = southPole S84
            let p2 = s84Pos 50 180 zero
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 0)
        it "returns the initial bearing in compass angle" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 12000)
            let p2 = s84Pos 58.643889 (-3.07) (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 9.1198173275)
        it "returns the initial bearing in compass angle" $ do
            let p1 = s84Pos 58.643889 (-3.07) (metres 12000)
            let p2 = s84Pos 50.066389 (-5.714722) (metres 5000)
            initialBearing p1 p2 `shouldBe` Just (decimalDegrees 191.27520031611112)
    describe "interpolate" $ do
        let p1 = s84Pos 44 44 zero
        let p2 = s84Pos 46 46 zero
        it "fails if f < 0.0" $
            evaluate (interpolate p1 p2 (-0.5)) `shouldThrow` errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (interpolate p1 p2 1.1) `shouldThrow` errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $ interpolate p1 p2 0.0 `shouldBe` p1
        it "returns p1 if f == 1" $ interpolate p1 p2 1.0 `shouldBe` p2
        it "returns the interpolated position" $ do
            let p3 = s84Pos 53.479444 (-2.245278) (metres 10000)
            let p4 = s84Pos 55.605833 13.035833 (metres 20000)
            interpolate p3 p4 0.5 `shouldBe` s84Pos 54.78355703138889 5.194985318055555 (metres 15000)
    describe "isInsideSurface" $ do
        let p1 = s84Pos 45 1 zero
        let p2 = s84Pos 45 2 zero
        let p3 = s84Pos 46 1 zero
        let p4 = s84Pos 46 2 zero
        let p5 = s84Pos 45.1 1.1 zero
        let malmo = s84Pos 55.6050 13.0038 zero
        let ystad = s84Pos 55.4295 13.82 zero
        let lund = s84Pos 55.7047 13.1910 zero
        let helsingborg = s84Pos 56.0465 12.6945 zero
        let kristianstad = s84Pos 56.0294 14.1567 zero
        let hoor = s84Pos 55.9295 13.5297 zero
        let hassleholm = s84Pos 56.1589 13.7668 zero
        let copenhagen = s84Pos 55.6761 12.5683 zero
        it "return False if polygon is empty" $ isInsideSurface p1 [] `shouldBe` False
        it "return False if polygon does not define at least a triangle" $ isInsideSurface p1 [p1, p2] `shouldBe` False
        it "returns True if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            isInsideSurface p5 polygon `shouldBe` True
        it "returns False if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            let p = antipode p5
            isInsideSurface p polygon `shouldBe` False
        it "returns False if position is a vertex of the polygon" $ do
            let convex = [p1, p2, p4, p3]
            fmap (\p -> isInsideSurface p convex) convex `shouldBe` replicate 4 False
            let concave = [malmo, ystad, kristianstad, helsingborg, lund]
            fmap (\p -> isInsideSurface p concave) concave `shouldBe` replicate 5 False
        it "handles closed polygons" $ do
            let polygon = [p1, p2, p4, p3, p1]
            isInsideSurface p5 polygon `shouldBe` True
        it "handles concave polygons" $ do
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            isInsideSurface hoor polygon `shouldBe` True
            isInsideSurface hassleholm polygon `shouldBe` False
        it "considers a point on an edge to be in one polygon only" $ do
            let i = interpolate helsingborg lund 0.5
            let poly1 = [malmo, kristianstad, helsingborg, lund]
            let poly2 = [helsingborg, lund, copenhagen]
            isInsideSurface i poly1 `shouldBe` True
            isInsideSurface i poly2 `shouldBe` False
    describe "intersection" $ do
        it "returns nothing if both great arc are equals" $ do
            let a = minorArc (s84Pos 51.885 0.235 zero) (s84Pos 52.885 1.235 zero)
            join (intersection <$> a <*> a) `shouldBe` Nothing
        it "returns nothing if both great arc are equals (opposite orientation)" $ do
            let a1 = minorArc (s84Pos 51.885 0.235 zero) (s84Pos 52.885 1.235 zero)
            let a2 = minorArc (s84Pos 52.885 1.235 zero) (s84Pos 51.885 0.235 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside either great arc" $ do
            let a1 = minorArc (s84Pos 0 0 zero) (s84Pos 0 10 zero)
            let a2 = minorArc (s84Pos (-5) 5 zero) (s84Pos (-1) 5 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside both great arcs" $ do
            let a1 = minorArc (s84Pos 0 (-10) zero) (s84Pos 0 (-1) zero)
            let a2 = minorArc (s84Pos (-5) 5 zero) (s84Pos (-1) 5 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns the point where the two great arcs intersect" $ do
            let a1 = minorArc (s84Pos 51.885 0.235 zero) (s84Pos 48.269 13.093 zero)
            let a2 = minorArc (s84Pos 49.008 2.549 zero) (s84Pos 56.283 11.304 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Just (s84Pos 50.901738961111114 4.49418117 zero)
        it "handles a minor arc across the equator" $ do
            let a1 = minorArc (s84Pos 54 154 zero) (s84Pos (-54) 154 zero)
            let a2 = minorArc (s84Pos 53 153 zero) (s84Pos 53 155 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Just (s84Pos 53.00419442027778 154 zero)
        it "returns the common start position between the 2 minor arcs" $ do
            let a1 = minorArc (s84Pos (-41.52) 141 zero) (s84Pos (-65.444811) 111.616598 zero)
            let a2 = minorArc (s84Pos (-42.35) 141 zero) (s84Pos (-39.883333) 141 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Just (s84Pos (-41.52) 141.0 zero)
        it "returns the common end position between the 2 minor arcs" $ do
            let a1 = minorArc (s84Pos (-65.444811) 111.616598 zero) (s84Pos (-41.52) 141 zero)
            let a2 = minorArc (s84Pos (-39.883333) 141 zero) (s84Pos (-41.52) 141 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Just (s84Pos (-41.52) 141.0 zero)
        it "handles an intersection exactly on one of the minor arcs" $ do
            let a1 = minorArc (s84Pos 0 (-10) zero) (s84Pos 0 10 zero)
            let a2 = minorArc (s84Pos (-10) 0 zero) (s84Pos 10 0 zero)
            join (intersection <$> a1 <*> a2) `shouldBe` Just (s84Pos 0 0 zero)
    describe "intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc = greatCircleHeadingOn (s84Pos 51.885 0.235 zero) (decimalDegrees 108.63)
            intersections gc gc `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 = greatCircleThrough (s84Pos 51.885 0.235 zero) (s84Pos 52.885 1.235 zero)
            let gc2 = greatCircleThrough (s84Pos 52.885 1.235 zero) (s84Pos 51.885 0.235 zero)
            join (intersections <$> gc1 <*> gc2) `shouldBe` Nothing
        it "returns the two positions where the two great circles intersects" $ do
            let gc1 = greatCircleHeadingOn (s84Pos 51.885 0.235 zero) (decimalDegrees 108.63)
            let gc2 = greatCircleHeadingOn (s84Pos 49.008 2.549 zero) (decimalDegrees 32.72)
            let (i1, i2) = fromJust (intersections gc1 gc2)
            i1 `shouldBe` s84Pos 50.90172260888889 4.494278278888889 zero
            i2 `shouldBe` antipode i1
    describe "mean" $ do
        it "returns Nothing if no position is given" $ (mean [] :: (Maybe (Position S84))) `shouldBe` Nothing
        it "returns the unique given position" $ do
            let p = s84Pos 50.066389 (-5.714722) zero
            mean [p] `shouldBe` Just p
        it "returns the geographical mean" $ do
            let p1 = s84Pos 50.066389 (-5.714722) (metres 15000.0)
            let p2 = s84Pos 58.643889 (-3.07) (metres 25000.0)
            let e = s84Pos 54.3622869375 (-4.530672405) zero
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
