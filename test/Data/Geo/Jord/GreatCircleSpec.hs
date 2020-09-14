module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Control.Exception.Base (evaluate)
import Control.Monad (join)
import Data.Maybe (fromJust)

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Geodetic as Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.Math3d as Math3d (cross)
import Data.Geo.Jord.Models (S84(..))
import Data.Geo.Jord.Places

spec :: Spec
spec = do
    describe "alongTrackDistance" $ do
        it "returns a positive length when position is ahead start of great arc" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972)
            let g =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 53.3206 (-1.7297))
                        (Geodetic.s84Pos 53.1887 0.1334)
            fmap (GreatCircle.alongTrackDistance p) g `shouldBe` Just (Length.kilometres 62.3315791)
        it "returns a negative length when position is ahead start of great arc" $ do
            let p = Geodetic.s84Pos 53.3206 (-1.7297)
            let g =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 53.2611 (-0.7972))
                        (Geodetic.s84Pos 53.1887 0.1334)
            fmap (GreatCircle.alongTrackDistance p) g `shouldBe`
                Just (Length.kilometres (-62.329309979))
        it "returns 0 when position is start of great arc" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972)
            let g = GreatCircle.minorArc p (Geodetic.s84Pos 53.1887 0.1334)
            fmap (GreatCircle.alongTrackDistance p) g `shouldBe` Just Length.zero
    describe "crossTrackDistance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972)
            let gc =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 53.3206 (-1.7297))
                        (Angle.decimalDegrees 96.0)
            GreatCircle.crossTrackDistance p gc `shouldBe` Length.metres (-305.665267)
        it "returns a negative length when position is left of great circle" $ do
            let p = Geodetic.s84Pos 53.2611 (-0.7972)
            let gc =
                    GreatCircle.through
                        (Geodetic.s84Pos 53.3206 (-1.7297))
                        (Geodetic.s84Pos 53.1887 0.1334)
            fmap (GreatCircle.crossTrackDistance p) gc `shouldBe` Just (Length.metres (-307.549992))
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = Geodetic.s84Pos 53.261111 (-1.797222)
            let gc =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 53.320556 (-1.729722))
                        (Angle.decimalDegrees 96.02166667)
            GreatCircle.crossTrackDistance p gc `shouldBe` Length.metres 7042.396068
        it "returns a positive length when position is left of great circle" $ do
            let p = Geodetic.antipode (Geodetic.s84Pos 53.2611 (-0.7972))
            let gc =
                    GreatCircle.through
                        (Geodetic.s84Pos 53.3206 (-1.7297))
                        (Geodetic.s84Pos 53.1887 0.1334)
            fmap (GreatCircle.crossTrackDistance p) gc `shouldBe` Just (Length.metres 307.549992)
        it "return zero when position is on the great circle" $ do
            let gc1 = Geodetic.s84Pos 53.3206 (-1.7297)
            let gc2 = Geodetic.s84Pos 53.1887 (0.1334)
            let gc = fromJust $ GreatCircle.through gc1 gc2
            let ps =
                    fmap
                        (\f -> GreatCircle.interpolated gc1 gc2 f)
                        (take 11 (iterate (\x -> x + 0.1 :: Double) 0.0))
            fmap (\p -> GreatCircle.crossTrackDistance p gc) ps `shouldBe`
                (replicate 11 Length.zero)
    describe "destination" $ do
        it "return the given position if distance is 0 meter" $ do
            let p0 = Geodetic.s84Pos 53.320556 (-1.729722)
            GreatCircle.destination p0 (Angle.decimalDegrees 96.0217) Length.zero `shouldBe` p0
        it "return the position along the great circle at distance and bearing" $ do
            let p0 = Geodetic.s84Pos 53.320556 (-1.729722)
            let p1 = Geodetic.s84Pos 53.18826954833333 0.13327449055555557
            GreatCircle.destination p0 (Angle.decimalDegrees 96.0217) (Length.metres 124800) `shouldBe`
                p1
    describe "distance" $ do
        it "returns 0 if both points are equal" $ do
            let p = Geodetic.s84Pos 50.066389 (-5.714722)
            GreatCircle.distance p p `shouldBe` Length.zero
        it "returns the distance between 2 points" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722)
            let p2 = Geodetic.s84Pos 58.643889 (-3.07)
            GreatCircle.distance p1 p2 `shouldBe` Length.metres 968854.878007
        it "handles singularity at the pole" $
            GreatCircle.distance (Geodetic.northPole S84) (Geodetic.southPole S84) `shouldBe`
            Length.kilometres 20015.114352233
        it "handles the discontinuity at the Date Line" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-179.999722)
            let p2 = Geodetic.s84Pos 50.066389 179.999722
            GreatCircle.distance p1 p2 `shouldBe` Length.metres 39.685092
    describe "greatCircle through position" $
        it "fails if both positions are equal" $
        GreatCircle.through (Geodetic.s84Pos 3 154) (Geodetic.s84Pos 3 154) `shouldBe` Nothing
    describe "finalBearing" $ do
        it "returns the Nothing if both positions are the same" $ do
            let p = Geodetic.s84Pos 50.066389 (-5.714722)
            GreatCircle.finalBearing p p `shouldBe` Nothing
            GreatCircle.finalBearing p (Geodetic.s84Pos 50.066389 (-5.714722)) `shouldBe` Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722)
            let p2 = Geodetic.s84Pos 58.643889 (-5.714722)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-5.714722)
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = Geodetic.s84Pos 0 0
            let p2 = Geodetic.s84Pos 0 1
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = Geodetic.s84Pos 0 1
            let p2 = Geodetic.s84Pos 0 0
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 270)
        it "returns the final bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722)
            let p2 = Geodetic.s84Pos 58.643889 (-3.07)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 11.27520031611111)
        it "returns the final bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-3.07)
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722)
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 189.1198173275)
        it "returns the final bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos (-53.994722) (-25.9875)
            let p2 = Geodetic.s84Pos 54 154
            GreatCircle.finalBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 125.68508662305555)
    describe "initialBearing" $ do
        it "returns Nothing if both positions are the same" $ do
            let p = Geodetic.s84Pos 50.066389 (-179.999722)
            GreatCircle.initialBearing p p `shouldBe` Nothing
            GreatCircle.initialBearing p (Geodetic.s84Pos 50.066389 (-179.999722)) `shouldBe`
                Nothing
        it "returns 0° if both positions have the same longitude (going north)" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722)
            let p2 = Geodetic.s84Pos 58.643889 (-5.714722)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° if both positions have the same longitude (going south)" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-5.714722)
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns 90° at the equator going east" $ do
            let p1 = Geodetic.s84Pos 0 0
            let p2 = Geodetic.s84Pos 0 1
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 90)
        it "returns 270° at the equator going west" $ do
            let p1 = Geodetic.s84Pos 0 1
            let p2 = Geodetic.s84Pos 0 0
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 270)
        it "returns 0° at the prime meridian going north" $ do
            let p1 = Geodetic.s84Pos 50 0
            let p2 = Geodetic.s84Pos 58 0
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° at the prime meridian going south" $ do
            let p1 = Geodetic.s84Pos 58 0
            let p2 = Geodetic.s84Pos 50 0
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns 0° at the date line going north" $ do
            let p1 = Geodetic.s84Pos 50 180
            let p2 = Geodetic.s84Pos 58 180
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.zero)
        it "returns 180° at the date line going south" $ do
            let p1 = Geodetic.s84Pos 58 180
            let p2 = Geodetic.s84Pos 50 180
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
            let p2 = Geodetic.s84Pos 50 180
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 180)
        it "returns the initial bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722)
            let p2 = Geodetic.s84Pos 58.643889 (-3.07)
            GreatCircle.initialBearing p1 p2 `shouldBe` Just (Angle.decimalDegrees 9.1198173275)
        it "returns the initial bearing in compass angle" $ do
            let p1 = Geodetic.s84Pos 58.643889 (-3.07)
            let p2 = Geodetic.s84Pos 50.066389 (-5.714722)
            GreatCircle.initialBearing p1 p2 `shouldBe`
                Just (Angle.decimalDegrees 191.27520031611112)
    describe "interpolated" $ do
        let p1 = Geodetic.s84Pos 44 44
        let p2 = Geodetic.s84Pos 46 46
        it "fails if f < 0.0" $
            evaluate (GreatCircle.interpolated p1 p2 (-0.5)) `shouldThrow`
            errorCall "fraction must be in range [0..1], was -0.5"
        it "fails if f > 1.0" $
            evaluate (GreatCircle.interpolated p1 p2 1.1) `shouldThrow`
            errorCall "fraction must be in range [0..1], was 1.1"
        it "returns p0 if f == 0" $ GreatCircle.interpolated p1 p2 0.0 `shouldBe` p1
        it "returns p1 if f == 1" $ GreatCircle.interpolated p1 p2 1.0 `shouldBe` p2
        it "returns the interpolated position" $ do
            let p3 = Geodetic.s84Pos 53.479444 (-2.245278)
            let p4 = Geodetic.s84Pos 55.605833 13.035833
            GreatCircle.interpolated p3 p4 0.5 `shouldBe`
                Geodetic.s84Pos 54.78355703138889 5.194985318055555
    describe "enclosedBy" $ do
        let p1 = Geodetic.s84Pos 45 1
        let p2 = Geodetic.s84Pos 45 2
        let p3 = Geodetic.s84Pos 46 1
        let p4 = Geodetic.s84Pos 46 2
        let p5 = Geodetic.s84Pos 45.1 1.1
        it "return False if polygon is empty" $ GreatCircle.enclosedBy p1 [] `shouldBe` False
        it "return False if polygon does not define at least a triangle" $
            GreatCircle.enclosedBy p1 [p1, p2] `shouldBe` False
        it "returns True if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            GreatCircle.enclosedBy p5 polygon `shouldBe` True
        it "returns False if position is inside polygon" $ do
            let polygon = [p1, p2, p4, p3]
            let p = Geodetic.antipode p5
            GreatCircle.enclosedBy p polygon `shouldBe` False
        it "returns False if position is a vertex of the polygon" $ do
            let convex = [p1, p2, p4, p3]
            fmap (\p -> GreatCircle.enclosedBy p convex) convex `shouldBe` replicate 4 False
            let concave = [malmo, ystad, kristianstad, helsingborg, lund]
            fmap (\p -> GreatCircle.enclosedBy p concave) concave `shouldBe` replicate 5 False
        it "handles closed polygons" $ do
            let polygon = [p1, p2, p4, p3, p1]
            GreatCircle.enclosedBy p5 polygon `shouldBe` True
        it "handles concave polygons" $ do
            let polygon = [malmo, ystad, kristianstad, helsingborg, lund]
            GreatCircle.enclosedBy hoor polygon `shouldBe` True
            GreatCircle.enclosedBy hassleholm polygon `shouldBe` False
        it "considers a point on an edge to be in one polygon only" $ do
            let i = GreatCircle.interpolated helsingborg lund 0.5
            let poly1 = [malmo, kristianstad, helsingborg, lund]
            let poly2 = [helsingborg, lund, copenhagen]
            GreatCircle.enclosedBy i poly1 `shouldBe` True
            GreatCircle.enclosedBy i poly2 `shouldBe` False
    describe "intersection" $ do
        it "returns nothing if both great arc are equals" $ do
            let a =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 51.885 0.235)
                        (Geodetic.s84Pos 52.885 1.235)
            join (GreatCircle.intersection <$> a <*> a) `shouldBe` Nothing
        it "returns nothing if both great arc are equals (opposite orientation)" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 51.885 0.235)
                        (Geodetic.s84Pos 52.885 1.235)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 52.885 1.235)
                        (Geodetic.s84Pos 51.885 0.235)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside either great arc" $ do
            let a1 = GreatCircle.minorArc (Geodetic.s84Pos 0 0) (Geodetic.s84Pos 0 10)
            let a2 = GreatCircle.minorArc (Geodetic.s84Pos (-5) 5) (Geodetic.s84Pos (-1) 5)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns nothing if great circle intersection is outside both great arcs" $ do
            let a1 = GreatCircle.minorArc (Geodetic.s84Pos 0 (-10)) (Geodetic.s84Pos 0 (-1))
            let a2 = GreatCircle.minorArc (Geodetic.s84Pos (-5) 5) (Geodetic.s84Pos (-1) 5)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe` Nothing
        it "returns the point where the two great arcs intersect" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 51.885 0.235)
                        (Geodetic.s84Pos 48.269 13.093)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos 49.008 2.549)
                        (Geodetic.s84Pos 56.283 11.304)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos 50.901738961111114 4.49418117)
        it "handles a minor arc across the equator" $ do
            let a1 = GreatCircle.minorArc (Geodetic.s84Pos 54 154) (Geodetic.s84Pos (-54) 154)
            let a2 = GreatCircle.minorArc (Geodetic.s84Pos 53 153) (Geodetic.s84Pos 53 155)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos 53.00419442027778 154)
        it "returns the common start position between the 2 minor arcs" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-41.52) 141)
                        (Geodetic.s84Pos (-65.444811) 111.616598)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-42.35) 141)
                        (Geodetic.s84Pos (-39.883333) 141)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos (-41.52) 141.0)
        it "returns the common end position between the 2 minor arcs" $ do
            let a1 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-65.444811) 111.616598)
                        (Geodetic.s84Pos (-41.52) 141)
            let a2 =
                    GreatCircle.minorArc
                        (Geodetic.s84Pos (-39.883333) 141)
                        (Geodetic.s84Pos (-41.52) 141)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe`
                Just (Geodetic.s84Pos (-41.52) 141.0)
        it "handles an intersection exactly on one of the minor arcs" $ do
            let a1 = GreatCircle.minorArc (Geodetic.s84Pos 0 (-10)) (Geodetic.s84Pos 0 10)
            let a2 = GreatCircle.minorArc (Geodetic.s84Pos (-10) 0) (Geodetic.s84Pos 10 0)
            join (GreatCircle.intersection <$> a1 <*> a2) `shouldBe` Just (Geodetic.s84Pos 0 0)
    describe "intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 51.885 0.235)
                        (Angle.decimalDegrees 108.63)
            GreatCircle.intersections gc gc `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 =
                    GreatCircle.through
                        (Geodetic.s84Pos 51.885 0.235)
                        (Geodetic.s84Pos 52.885 1.235)
            let gc2 =
                    GreatCircle.through
                        (Geodetic.s84Pos 52.885 1.235)
                        (Geodetic.s84Pos 51.885 0.235)
            join (GreatCircle.intersections <$> gc1 <*> gc2) `shouldBe` Nothing
        it "returns the two positions where the two great circles intersects" $ do
            let gc1 =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 51.885 0.235)
                        (Angle.decimalDegrees 108.63)
            let gc2 =
                    GreatCircle.headingOn
                        (Geodetic.s84Pos 49.008 2.549)
                        (Angle.decimalDegrees 32.72)
            let (i1, i2) = fromJust (GreatCircle.intersections gc1 gc2)
            i1 `shouldBe` Geodetic.s84Pos 50.90172260888889 4.494278278888889
            i2 `shouldBe` Geodetic.antipode i1
    describe "mean" $ do
        it "returns Nothing if no position is given" $
            (GreatCircle.mean [] :: (Maybe (HorizontalPosition S84))) `shouldBe` Nothing
        it "returns the unique given position" $ do
            let p = Geodetic.s84Pos 50.066389 (-5.714722)
            GreatCircle.mean [p] `shouldBe` Just p
        it "returns the geographical mean" $ do
            let p1 = Geodetic.s84Pos 50.066389 (-5.714722)
            let p2 = Geodetic.s84Pos 58.643889 (-3.07)
            let e = Geodetic.s84Pos 54.3622869375 (-4.530672405)
            GreatCircle.mean [p1, p2] `shouldBe` Just e
        it "returns Nothing if list contains antipodal positions" $ do
            let points =
                    [ Geodetic.s84Pos 45 1
                    , Geodetic.s84Pos 45 2
                    , Geodetic.s84Pos 46 2
                    , Geodetic.s84Pos 46 1
                    , Geodetic.antipode (Geodetic.s84Pos 45 2)
                    ]
            GreatCircle.mean points `shouldBe` Nothing
    describe "projection" $ do
        it "returns Nothing if position is the normal to minor arc (1/2)" $ do
            let s = Geodetic.s84Pos 3 (-10)
            let e = (Geodetic.s84Pos 4 10)
            let ma = fromJust (GreatCircle.minorArc s e)
            let p =
                    Geodetic.nvectorPos'
                        (Math3d.cross (Geodetic.nvector s) (Geodetic.nvector e))
                        S84
            GreatCircle.projection p ma `shouldBe` Nothing
        it "returns Nothing if position is the normal to minor arc (2/2)" $ do
            let ma =
                    fromJust (GreatCircle.minorArc (Geodetic.s84Pos 0 (-10)) (Geodetic.s84Pos 0 10))
            let p = Geodetic.northPole S84
            GreatCircle.projection p ma `shouldBe` Nothing
        it "returns Nothing if position is the antipode of the normal to minor arc (1/2)" $ do
            let s = Geodetic.s84Pos 3 (-10)
            let e = (Geodetic.s84Pos 4 10)
            let ma = fromJust (GreatCircle.minorArc s e)
            let p =
                    Geodetic.antipode
                        (Geodetic.nvectorPos'
                             (Math3d.cross (Geodetic.nvector s) (Geodetic.nvector e))
                             S84)
            GreatCircle.projection p ma `shouldBe` Nothing
        it "returns Nothing if position is the antipode of the normal to minor arc (2/2)" $ do
            let ma =
                    fromJust (GreatCircle.minorArc (Geodetic.s84Pos 0 (-10)) (Geodetic.s84Pos 0 10))
            let p = Geodetic.southPole S84
            GreatCircle.projection p ma `shouldBe` Nothing
        it "returns Nothing if projection is outside minor arc" $ do
            let ma = fromJust (GreatCircle.minorArc (Geodetic.s84Pos 54 15) (Geodetic.s84Pos 54 20))
            let p = Geodetic.s84Pos 54 10
            GreatCircle.projection p ma `shouldBe` Nothing
        it "returns the projection if within the minor arc" $ do
            let s = Geodetic.s84Pos 53.3206 (-1.7297)
            let e = Geodetic.s84Pos 53.1887 0.1334
            let ma = fromJust (GreatCircle.minorArc s e)
            let p = Geodetic.s84Pos 53.2611 (-0.7972)
            let proj = GreatCircle.projection p ma
            proj `shouldBe` Just (Geodetic.s84Pos 53.25835330666666 (-0.7977433863888889))
            -- absolute cross track distance from p to great circle should be distance between projection and p
            let stx = (GreatCircle.crossTrackDistance p (fromJust (GreatCircle.through s e)))
            abs (Length.toMetres stx) `shouldBe`
                Length.toMetres (GreatCircle.distance (fromJust proj) p)
        it "handles p exactly being the start of the minor arc (1/2)" $ do
            let s = Geodetic.s84Pos 54 15
            let ma = fromJust (GreatCircle.minorArc s (Geodetic.s84Pos 54 20))
            GreatCircle.projection s ma `shouldBe` (Just s)
        it "handles p exactly being the start of the minor arc (2/2)" $ do
            let s = Geodetic.s84Pos 13.733333587646484 100.5
            let ma = fromJust (GreatCircle.minorArc s (Geodetic.s84Pos 12.0 100.58499908447266))
            GreatCircle.projection s ma `shouldBe` (Just s)
        it "handles p exactly being the end of the minor arc (1/2)" $ do
            let e = Geodetic.s84Pos 54 20
            let ma = fromJust (GreatCircle.minorArc (Geodetic.s84Pos 54 15) e)
            GreatCircle.projection e ma `shouldBe` (Just e)
        it "handles p exactly being the end of the minor arc (2/2)" $ do
            let e = Geodetic.s84Pos 12.0 100.58499908447266
            let ma = fromJust (GreatCircle.minorArc (Geodetic.s84Pos 13.733333587646484 100) e)
            GreatCircle.projection e ma `shouldBe` (Just e)
    describe "side" $ do
        it "retuns None if p1 is antipode of p2" $ do
            GreatCircle.side ystad helsingborg (Geodetic.antipode helsingborg) `shouldBe`
                GreatCircle.None
        it "returns None if (p1, p2) are equal" $ do
            GreatCircle.side ystad helsingborg helsingborg `shouldBe` GreatCircle.None
        it "returns None if p0 is on the great circle" $ do
            GreatCircle.side (Geodetic.s84Pos 0 0) (Geodetic.s84Pos 45 0) (Geodetic.northPole S84) `shouldBe`
                GreatCircle.None
            GreatCircle.side helsingborg helsingborg kristianstad `shouldBe` GreatCircle.None
            GreatCircle.side kristianstad helsingborg kristianstad `shouldBe` GreatCircle.None
        it "return LeftOf or RightOf" $ do
            GreatCircle.side ystad helsingborg kristianstad `shouldBe` GreatCircle.RightOf
            GreatCircle.side ystad kristianstad helsingborg `shouldBe` GreatCircle.LeftOf
            GreatCircle.side malmo lund helsingborg `shouldBe` GreatCircle.LeftOf
            GreatCircle.side malmo helsingborg lund `shouldBe` GreatCircle.RightOf
    describe "turn" $ do
        it "returns a negative angle when turning left" $ do
            GreatCircle.turn (Geodetic.s84Pos 0 0) (Geodetic.s84Pos 45 0) (Geodetic.s84Pos 60 (-10)) `shouldBe`
                Angle.decimalDegrees 18.192705871944444
        it "returns a positive angle when turning right" $ do
            GreatCircle.turn (Geodetic.s84Pos 0 0) (Geodetic.s84Pos 45 0) (Geodetic.s84Pos 60 10) `shouldBe`
                Angle.decimalDegrees (-18.192705871944444)
        it "returns 0 when a, b & c are aligned" $ do
            GreatCircle.turn (Geodetic.s84Pos 0 0) (Geodetic.s84Pos 45 0) (Geodetic.northPole S84) `shouldBe`
                Angle.zero
        it "returns 0 is any 2 of the 3 positions are equal" $ do
            let a = Geodetic.s84Pos 45 63
            let b = Geodetic.s84Pos (-54) (-89)
            GreatCircle.turn a a a `shouldBe` Angle.zero
            GreatCircle.turn a a b `shouldBe` Angle.zero
            GreatCircle.turn a b b `shouldBe` Angle.zero
            GreatCircle.turn b b b `shouldBe` Angle.zero
            GreatCircle.turn b b a `shouldBe` Angle.zero
            GreatCircle.turn b a a `shouldBe` Angle.zero
