module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Control.Exception.Base (evaluate)
import Control.Monad (join)
import Data.Maybe (fromJust)

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Models (S84(..))

spec :: Spec
spec = do
    describe "alongTrackDistance" $ do
        it "returns a positive length when position is ahead start of great arc" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972) Length.zero
            let g =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 53.3206 (-1.7297) Length.zero)
                        (Geodetic.s84Pos 53.1887 0.1334 Length.zero)
            fmap (GreatCircle.alongTrackDistance p) g `shouldBe` Just (Length.kilometres 62.3315791)
        it "returns a negative length when position is ahead start of great arc" $ do
            let p = Geodetic.s84Pos 53.3206 (-1.7297) Length.zero
            let g =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 53.2611 (-0.7972) Length.zero)
                        (Geodetic.s84Pos 53.1887 0.1334 Length.zero)
            fmap (GreatCircle.alongTrackDistance p) g `shouldBe`
                Just (Length.kilometres (-62.329309979))
        it "returns 0 when position is start of great arc" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972) Length.zero
            let g = GreatCircle.minorArc p (Geodetic.s84Pos 53.1887 0.1334 Length.zero)
            fmap (GreatCircle.alongTrackDistance p) g `shouldBe` Just Length.zero
    describe "crossTrackDistance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972) Length.zero
            let gc =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 53.3206 (-1.7297) Length.zero)
                        (Angle.decimalDegrees 96.0)
            GreatCircle.crossTrackDistance p gc `shouldBe` Length.metres (-305.665267)
        it "returns a negative length when position is left of great circle" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972) Length.zero
            let gc =
                    GreatCircle.through
                        (Geodetic.s84Pos 53.3206 (-1.7297) Length.zero)
                        (Geodetic.s84Pos 53.1887 0.1334 Length.zero)
            fmap (GreatCircle.crossTrackDistance p) gc `shouldBe` Just (Length.metres (-307.549992))
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = Geodetic.s84Pos 53.261111 (-1.797222) Length.zero
            let gc =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 53.320556 (-1.729722) Length.zero)
                        (Angle.decimalDegrees 96.02166667)
            GreatCircle.crossTrackDistance p gc `shouldBe` Length.metres 7042.396068
        it "returns a positive length when position is left of great circle" $ do
            let p = Geodetic.antipode (Geodetic.s84Pos 53.2611 (-0.7972) Length.zero)
            let gc =
                    GreatCircle.through
                        (Geodetic.s84Pos 53.3206 (-1.7297) Length.zero)
                        (Geodetic.s84Pos 53.1887 0.1334 Length.zero)
            fmap (GreatCircle.crossTrackDistance p) gc `shouldBe` Just (Length.metres 307.549992)
        it "return zero when position is on the great circle" $ do
            let gc1 = Geodetic.s84Pos 53.3206 (-1.7297) Length.zero
            let gc2 = Geodetic.s84Pos 53.1887 (0.1334) Length.zero
            let gc = fromJust $ GreatCircle.through gc1 gc2
            let ps =
                    fmap
                        (\f -> GreatCircle.interpolate gc1 gc2 f)
                        (take 11 (iterate (\x -> x + 0.1 :: Double) 0.0))
            fmap (\p -> GreatCircle.crossTrackDistance p gc) ps `shouldBe`
                (replicate 11 Length.zero)
    describe "destination" $ do
        it "return the given position if distance is 0 meter" $ do
            let p0 = Geodetic.s84Pos 53.320556 (-1.729722) Length.zero
            GreatCircle.destination p0 (Angle.decimalDegrees 96.0217) Length.zero `shouldBe` p0
        it "return the position along the great circle at distance and bearing" $ do
            let p0 = Geodetic.s84Pos 53.320556 (-1.729722) (Length.metres 15000.0)
            let p1 = Geodetic.s84Pos 53.18826954833333 0.13327449055555557 (Length.metres 15000.0)
            GreatCircle.destination p0 (Angle.decimalDegrees 96.0217) (Length.metres 124800) `shouldBe`
                p1
    describe "surfaceDistance" $ do
        it "returns 0 if both points are equal" $ do
            let p = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 15000.0)
            GreatCircle.surfaceDistance p p `shouldBe` Length.zero
        it "returns the distance between 2 points" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722) Length.zero
            let p2 = Geodetic.s84Pos 58.643889 (-3.07) Length.zero
            GreatCircle.surfaceDistance p1 p2 `shouldBe` Length.metres 968854.878007
        it "handles singularity at the pole" $
            GreatCircle.surfaceDistance (Geodetic.northPole S84) (Geodetic.southPole S84) `shouldBe`
            Length.kilometres 20015.114352233
        it "handles the discontinuity at the Date Line" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-179.999722) Length.zero
            let p2 = Geodetic.s84Pos 50.066389 179.999722 Length.zero
            GreatCircle.surfaceDistance p1 p2 `shouldBe` Length.metres 39.685092
    describe "greatCircle through position" $
        it "fails if both positions are equal" $
        GreatCircle.through (Geodetic.s84Pos 3 154 Length.zero) (Geodetic.s84Pos 3 154 Length.zero) `shouldBe`
        Nothing
    describe "finalBearing" $ do
        it "returns the Nothing if both positions are the same (ignoring height)" $ do
            let p = Geodetic.s84Pos 50.066389 (-5.714722) Length.zero
            GreatCircle.finalBearing p p `shouldBe` Nothing
            GreatCircle.finalBearing p (Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 10)) `shouldBe`
                Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 12000)
            let p2 = Geodetic.s84Pos 58.643889 (-5.714722) (Length.metres 5000)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-5.714722) (Length.metres 5000)
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 12000)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = Geodetic.s84Pos 0 0 (Length.metres 12000)
            let p2 = Geodetic.s84Pos 0 1 (Length.metres 5000)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = Geodetic.s84Pos 0 1 (Length.metres 12000)
            let p2 = Geodetic.s84Pos 0 0 (Length.metres 5000)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 270)
        it "returns the final bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722) Length.zero
            let p2 = Geodetic.s84Pos 58.643889 (-3.07) Length.zero
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 11.27520031611111)
        it "returns the final bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-3.07) Length.zero
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722) Length.zero
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 189.1198173275)
        it "returns the final bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos (-53.994722) (-25.9875) Length.zero
            let p2 = Geodetic.s84Pos 54 154 Length.zero
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 125.68508662305555)
    describe "initialBearing" $ do
        it "returns Nothing if both positions are the same (ignoring height)" $ do
            let p = Geodetic.s84Pos 50.066389 (-179.999722) Length.zero
            GreatCircle.initialBearing p p `shouldBe` Nothing
            GreatCircle.initialBearing
                p
                (Geodetic.s84Pos 50.066389 (-179.999722) (Length.metres 100)) `shouldBe`
                Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 12000)
            let p2 = Geodetic.s84Pos 58.643889 (-5.714722) (Length.metres 12000)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-5.714722) (Length.metres 12000)
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 12000)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = Geodetic.s84Pos 0 0 (Length.metres 12000)
            let p2 = Geodetic.s84Pos 0 1 (Length.metres 5000)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = Geodetic.s84Pos 0 1 (Length.metres 12000)
            let p2 = Geodetic.s84Pos 0 0 (Length.metres 5000)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 270)
        it "returns 0° at the prime meridian going north" $ do
            let p1 = Geodetic.s84Pos 50 0 Length.zero
            let p2 = Geodetic.s84Pos 58 0 Length.zero
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° at the prime meridian going south" $ do
            let p1 = Geodetic.s84Pos 58 0 Length.zero
            let p2 = Geodetic.s84Pos 50 0 Length.zero
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns 0° at the date line going north" $ do
            let p1 = Geodetic.s84Pos 50 180 Length.zero
            let p2 = Geodetic.s84Pos 58 180 Length.zero
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° at the date line going south" $ do
            let p1 = Geodetic.s84Pos 58 180 Length.zero
            let p2 = Geodetic.s84Pos 50 180 Length.zero
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns 0° going from the south pole to the north pole" $ do
            let p1 = Geodetic.southPole S84
            let p2 = Geodetic.northPole S84
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 0° going from the north pole to the south pole" $ do
            let p1 = Geodetic.northPole S84
            let p2 = Geodetic.southPole S84
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° going from the south pole to anywhere on the date line" $ do
            let p1 = Geodetic.southPole S84
            let p2 = Geodetic.s84Pos 50 180 Length.zero
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns the initial bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 12000)
            let p2 = Geodetic.s84Pos 58.643889 (-3.07) (Length.metres 5000)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 9.1198173275)
        it "returns the initial bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-3.07) (Length.metres 12000)
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 5000)
            GreatCircle.initialBearing p1 p2 `shouldBe`
                Just (Angle.decimalDegrees 191.27520031611112)
    describe "interpolate" $ do
        let p1 = Geodetic.s84Pos 44 44 Length.zero
        let p2 = Geodetic.s84Pos 46 46 Length.zero
        it "fails if f < 0.0" $
            evaluate (GreatCircle.interpolate p1 p2 (-0.5)) `shouldThrow`
            errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (GreatCircle.interpolate p1 p2 1.1) `shouldThrow`
            errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $ GreatCircle.interpolate p1 p2 0.0 `shouldBe` p1
        it "returns p1 if f == 1" $ GreatCircle.interpolate p1 p2 1.0 `shouldBe` p2
        it "returns the interpolated position" $ do
            let p3 = Geodetic.s84Pos 53.479444 (-2.245278) (Length.metres 10000)
            let p4 = Geodetic.s84Pos 55.605833 13.035833 (Length.metres 20000)
            GreatCircle.interpolate p3 p4 0.5 `shouldBe`
                Geodetic.s84Pos 54.78355703138889 5.194985318055555 (Length.metres 15000)
    describe "isInsideSurface" $ do
        let p1 = Geodetic.s84Pos 45 1 Length.zero
        let p2 = Geodetic.s84Pos 45 2 Length.zero
        let p3 = Geodetic.s84Pos 46 1 Length.zero
        let p4 = Geodetic.s84Pos 46 2 Length.zero
        let p5 = Geodetic.s84Pos 45.1 1.1 Length.zero
        let malmo = Geodetic.s84Pos 55.6050 13.0038 Length.zero
        let ystad = Geodetic.s84Pos 55.4295 13.82 Length.zero
        let lund = Geodetic.s84Pos 55.7047 13.1910 Length.zero
        let helsingborg = Geodetic.s84Pos 56.0465 12.6945 Length.zero
        let kristianstad = Geodetic.s84Pos 56.0294 14.1567 Length.zero
        let hoor = Geodetic.s84Pos 55.9295 13.5297 Length.zero
        let hassleholm = Geodetic.s84Pos 56.1589 13.7668 Length.zero
        let copenhagen = Geodetic.s84Pos 55.6761 12.5683 Length.zero
        it "return False if polygon is empty" $ GreatCircle.isInsideSurface p1 [] `shouldBe` False
        it "return False if polygon does not define at least a triangle" $
            GreatCircle.isInsideSurface p1 [p1, p2] `shouldBe` False
        it "returns True if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            GreatCircle.isInsideSurface p5 polygon `shouldBe` True
        it "returns False if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            let p = Geodetic.antipode p5
            GreatCircle.isInsideSurface p polygon `shouldBe` False
        it "returns False if position is a vertex of the polygon" $ do
            let convex = [p1, p2, p4, p3]
            fmap (\p -> GreatCircle.isInsideSurface p convex) convex `shouldBe` replicate 4 False
            let concave = [malmo, ystad, kristianstad, helsingborg, lund]
            fmap (\p -> GreatCircle.isInsideSurface p concave) concave `shouldBe` replicate 5 False
        it "handles closed polygons" $ do
            let polygon = [p1, p2, p4, p3, p1]
            GreatCircle.isInsideSurface p5 polygon `shouldBe` True
        it "handles concave polygons" $ do
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            GreatCircle.isInsideSurface hoor polygon `shouldBe` True
            GreatCircle.isInsideSurface hassleholm polygon `shouldBe` False
        it "considers a point on an edge to be in one polygon only" $ do
            let i = GreatCircle.interpolate helsingborg lund 0.5
            let poly1 = [malmo, kristianstad, helsingborg, lund]
            let poly2 = [helsingborg, lund, copenhagen]
            GreatCircle.isInsideSurface i poly1 `shouldBe` True
            GreatCircle.isInsideSurface i poly2 `shouldBe` False
    describe "intersection" $ do
        it "returns nothing if both great arc are equals" $ do
            let a =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
                        (Geodetic.s84Pos 52.885 1.235 Length.zero)
            join (GreatCircle.intersection <$> a <*> a) `shouldBe` Nothing
        it "returns nothing if both great arc are equals (opposite orientation)" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
                        (Geodetic.s84Pos 52.885 1.235 Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 52.885 1.235 Length.zero)
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside either great arc" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 0 0 Length.zero)
                        (Geodetic.s84Pos 0 10 Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-5) 5 Length.zero)
                        (Geodetic.s84Pos (-1) 5 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside both great arcs" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 0 (-10) Length.zero)
                        (Geodetic.s84Pos 0 (-1) Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-5) 5 Length.zero)
                        (Geodetic.s84Pos (-1) 5 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns the point where the two great arcs intersect" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
                        (Geodetic.s84Pos 48.269 13.093 Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 49.008 2.549 Length.zero)
                        (Geodetic.s84Pos 56.283 11.304 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos 50.901738961111114 4.49418117 Length.zero)
        it "handles a minor arc across the equator" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 54 154 Length.zero)
                        (Geodetic.s84Pos (-54) 154 Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 53 153 Length.zero)
                        (Geodetic.s84Pos 53 155 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos 53.00419442027778 154 Length.zero)
        it "returns the common start position between the 2 minor arcs" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-41.52) 141 Length.zero)
                        (Geodetic.s84Pos (-65.444811) 111.616598 Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-42.35) 141 Length.zero)
                        (Geodetic.s84Pos (-39.883333) 141 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos (-41.52) 141.0 Length.zero)
        it "returns the common end position between the 2 minor arcs" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-65.444811) 111.616598 Length.zero)
                        (Geodetic.s84Pos (-41.52) 141 Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-39.883333) 141 Length.zero)
                        (Geodetic.s84Pos (-41.52) 141 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos (-41.52) 141.0 Length.zero)
        it "handles an intersection exactly on one of the minor arcs" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 0 (-10) Length.zero)
                        (Geodetic.s84Pos 0 10 Length.zero)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-10) 0 Length.zero)
                        (Geodetic.s84Pos 10 0 Length.zero)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos 0 0 Length.zero)
    describe "intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
                        (Angle.decimalDegrees 108.63)
            GreatCircle.intersections gc gc `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 =
                    GreatCircle.through
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
                        (Geodetic.s84Pos 52.885 1.235 Length.zero)
            let gc2 =
                    GreatCircle.through
                        (Geodetic.s84Pos 52.885 1.235 Length.zero)
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
            join (GreatCircle.intersections <$> gc1 <*> gc2) `shouldBe` Nothing
        it "returns the two positions where the two great circles intersects" $ do
            let gc1 =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 51.885 0.235 Length.zero)
                        (Angle.decimalDegrees 108.63)
            let gc2 =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 49.008 2.549 Length.zero)
                        (Angle.decimalDegrees 32.72)
            let (i1, i2) = fromJust (GreatCircle.intersections gc1 gc2)
            i1 `shouldBe` Geodetic.s84Pos 50.90172260888889 4.494278278888889 Length.zero
            i2 `shouldBe` Geodetic.antipode i1
    describe "mean" $ do
        it "returns Nothing if no position is given" $
            (GreatCircle.mean [] :: (Maybe (Geodetic.Position S84))) `shouldBe` Nothing
        it "returns the unique given position" $ do
            let p = Geodetic.s84Pos 50.066389 (-5.714722) Length.zero
            GreatCircle.mean [p] `shouldBe` Just p
        it "returns the geographical mean" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722) (Length.metres 15000.0)
            let p2 = Geodetic.s84Pos 58.643889 (-3.07) (Length.metres 25000.0)
            let e = Geodetic.s84Pos 54.3622869375 (-4.530672405) Length.zero
            GreatCircle.mean [p1, p2] `shouldBe` Just e
        it "returns Nothing if list contains antipodal positions" $ do
            let points =
                    [ Geodetic.s84Pos 45 1 Length.zero
                    , Geodetic.s84Pos 45 2 Length.zero
                    , Geodetic.s84Pos 46 2 Length.zero
                    , Geodetic.s84Pos 46 1 Length.zero
                    , Geodetic.antipode (Geodetic.s84Pos 45 2 Length.zero)
                    ]
            GreatCircle.mean points `shouldBe` Nothing
