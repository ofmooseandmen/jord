module Data.Geo.Jord.PolygonSpec
    ( spec
    ) where

import Data.Either (fromRight, isRight)
import Data.Maybe (mapMaybe)

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Model (Spherical)
import Data.Geo.Jord.Places
import Data.Geo.Jord.Polygon (Error(..), Polygon)
import qualified Data.Geo.Jord.Polygon as Polygon
import Data.Geo.Jord.Triangle (Triangle)
import qualified Data.Geo.Jord.Triangle as Triangle

spec :: Spec
spec = do
    describe "simple" $ do
        it "returns an error if less than 3 vertices" $ do
            let ps = [Geodetic.s84Pos (-2) (-2), Geodetic.s84Pos 2 2]
            Polygon.simple ps `shouldBe` Left NotEnoughVertices
        it "returns an error if edges self-intersects" $ do
            let ps =
                    [ Geodetic.s84Pos (-2) (-2)
                    , Geodetic.s84Pos 2 (-2)
                    , Geodetic.s84Pos 3 0
                    , Geodetic.s84Pos (-2) 2
                    , Geodetic.s84Pos 2 2
                    ]
            Polygon.simple ps `shouldBe` Left SeflIntersectingEdge
        it "returns an error if edges self-intersects (quad)" $ do
            let ps =
                    [ Geodetic.s84Pos (-2) (-2)
                    , Geodetic.s84Pos 2 (-2)
                    , Geodetic.s84Pos (-2) 2
                    , Geodetic.s84Pos 2 2
                    ]
            Polygon.simple ps `shouldBe` Left SeflIntersectingEdge
        it "returns a concave polygon (4 vertices in clockwise order)" $ do
            let p = Polygon.simple [ystad, hoor, helsingborg, kristianstad]
            fmap Polygon.concave p `shouldBe` Right True
        it "returns a concave polygon (4 vertices in counterclockwise order)" $ do
            let p = Polygon.simple [ystad, kristianstad, helsingborg, hoor]
            fmap Polygon.concave p `shouldBe` Right True
        it "returns a concave polygon (5 vertices in clockwise order)" $ do
            let p = Polygon.simple [ystad, malmo, lund, helsingborg, kristianstad]
            fmap Polygon.concave p `shouldBe` Right True
        it "returns a concave polygon (5 vertices in counterclockwise order)" $ do
            let p = Polygon.simple [ystad, lund, kristianstad, helsingborg, malmo]
            fmap Polygon.concave p `shouldBe` Right True
        it "returns a concave polygon (7 vertices in clockwise order)" $ do
            let p =
                    Polygon.simple
                        [bangui, juba, djibouti, antananrivo, dar_es_salaam, kinshasa, narobi]
            fmap Polygon.concave p `shouldBe` Right True
        it "returns a convex polygon (4 vertices clockwise order)" $ do
            let p = Polygon.simple [ystad, malmo, helsingborg, kristianstad]
            fmap Polygon.concave p `shouldBe` Right False
        it "returns a convex polygon (4 vertices counterclockwise order)" $ do
            let p = Polygon.simple [ystad, kristianstad, helsingborg, malmo]
            fmap Polygon.concave p `shouldBe` Right False
        it "returns a convex polygon (6 vertices in clockwise order)" $ do
            let p = Polygon.simple [bangui, juba, narobi, dar_es_salaam, harare, kinshasa]
            fmap Polygon.concave p `shouldBe` Right False
        it "returns a convex polygon (triangle)" $ do
            let p = Polygon.simple [ystad, malmo, helsingborg]
            fmap Polygon.concave p `shouldBe` Right False
    describe "circle" $ do
        it "returns a error if radius <= 0" $ do
            Polygon.circle malmo Length.zero 10 `shouldBe` Left InvalidRadius
            Polygon.circle malmo (Length.metres (-1)) 10 `shouldBe` Left InvalidRadius
        it "returns a error if radius <= 0" $ do
            Polygon.circle malmo (Length.metres 1000) 2 `shouldBe` Left NotEnoughVertices
        it "returns a convex polygon" $ do
            let c = Geodetic.s84Pos 55.6050 13.0038
            let r = Length.metres 2000.0
            let nb = 10
            let n = fromIntegral nb :: Double
            let eBrngs = take nb (iterate (\x -> x + 360.0 / n) 0.0)
            let ep = Polygon.circle c r nb
            assertPoly ep c r nb eBrngs
    describe "arc" $ do
        it "returns an error if radius <= 0" $ do
            Polygon.arc malmo Length.zero Angle.zero Angle.zero 10 `shouldBe` Left InvalidRadius
            Polygon.arc malmo (Length.metres (-1)) Angle.zero Angle.zero 10 `shouldBe`
                Left InvalidRadius
        it "returns an error if radius <= 0" $ do
            Polygon.arc malmo (Length.metres 1000) Angle.zero Angle.zero 2 `shouldBe`
                Left NotEnoughVertices
        it "returns an error if start = end angle" $ do
            Polygon.arc
                malmo
                (Length.metres 1000)
                (Angle.decimalDegrees 154)
                (Angle.decimalDegrees 154)
                4 `shouldBe`
                Left EmptyArcRange
        it "returns a convex polygon (start < end)" $ do
            let c = Geodetic.s84Pos 55.6050 13.0038
            let r = Length.metres 2000.0
            let nb = 10
            let sa = Angle.decimalDegrees 36
            let ea = Angle.decimalDegrees 289
            let ep = Polygon.arc c r sa ea nb
            let n = fromIntegral nb :: Double
            let eBrngs = take nb (iterate (\x -> x + (289.0 - 36.0) / (n - 1.0)) 36.0)
            assertPoly ep c r nb eBrngs
        it "returns a convex polygon (start > end)" $ do
            let c = Geodetic.s84Pos 55.6050 13.0038
            let r = Length.metres 2000.0
            let nb = 10
            let sa = Angle.decimalDegrees 180
            let ea = Angle.decimalDegrees 90
            let ep = Polygon.arc c r sa ea nb
            let n = fromIntegral nb :: Double
            let eBrngs =
                    take
                        nb
                        (fmap
                             (\x ->
                                  if x >= 360.0
                                      then x - 360.0
                                      else x)
                             (iterate (\x -> x + 270.0 / (n - 1.0)) 180.0))
            assertPoly ep c r nb eBrngs
    describe "triangulate" $ do
        it "triangluates a concave clockwise quad (1/2)" $ do
            let p = simple [kristianstad, ystad, helsingborg, horby]
            let ts = Polygon.triangulate p
            ts `shouldBe` [triangle horby kristianstad ystad, triangle ystad helsingborg horby]
        it "triangluates a concave clockwise quad (2/2)" $ do
            let p = simple [ystad, helsingborg, kristianstad, horby]
            let ts = Polygon.triangulate p
            ts `shouldBe`
                [triangle horby ystad helsingborg, triangle helsingborg kristianstad horby]
        it "triangluates a concave clockwise pentagon" $ do
            let p = simple [ystad, malmo, lund, helsingborg, kristianstad]
            let ts = Polygon.triangulate p
            ts `shouldBe`
                [ triangle kristianstad ystad malmo
                , triangle kristianstad malmo lund
                , triangle lund helsingborg kristianstad
                ]
        it "triangluates a concave clockwise polygon with 7 vertices" $ do
            let p = simple [bangui, juba, djibouti, antananrivo, dar_es_salaam, kinshasa, narobi]
            let ts = Polygon.triangulate p
            ts `shouldBe`
                [ triangle narobi bangui juba
                , triangle narobi juba djibouti
                , triangle narobi djibouti antananrivo
                , triangle narobi antananrivo dar_es_salaam
                , triangle dar_es_salaam kinshasa narobi
                ]
        it "triangluates a concave counterclockwise pentagon" $ do
            let p = simple [ystad, lund, kristianstad, helsingborg, malmo]
            let ts = Polygon.triangulate p
            ts `shouldBe`
                [ triangle helsingborg kristianstad lund
                , triangle malmo helsingborg lund
                , triangle malmo lund ystad
                ]
        it "triangluates a convex clockwise quad" $ do
            let p = simple [ystad, malmo, helsingborg, kristianstad]
            let ts = Polygon.triangulate p
            ts `shouldBe`
                [triangle kristianstad ystad malmo, triangle malmo helsingborg kristianstad]
        it "triangulates a convex polygon with 6 vertices" $ do
            let p = simple [bangui, juba, narobi, dar_es_salaam, harare, kinshasa]
            let ts = Polygon.triangulate p
            ts `shouldBe`
                [ triangle kinshasa bangui juba
                , triangle kinshasa juba narobi
                , triangle kinshasa narobi dar_es_salaam
                , triangle dar_es_salaam harare kinshasa
                ]
        it "triangluates a convex counterclockwise quad" $ do
            let p = simple [ystad, kristianstad, helsingborg, malmo]
            let ts = Polygon.triangulate p
            ts `shouldBe`
                [triangle ystad malmo helsingborg, triangle helsingborg kristianstad ystad]
        it "triangluates a triangle" $ do
            let p = simple [ystad, malmo, helsingborg]
            let ts = Polygon.triangulate p
            ts `shouldBe` [triangle ystad malmo helsingborg]

assertPoly ::
       (Spherical a)
    => Either Error (Polygon a)
    -> HorizontalPosition a
    -> Length
    -> Int
    -> [Double]
    -> IO ()
assertPoly ep c r nb eBrngs = do
    isRight ep `shouldBe` True
    let p = fromRight (error "should be right") ep
    Polygon.concave p `shouldBe` False
    let vs = Polygon.vertices p
    length vs `shouldBe` nb
    let aDists = fmap (GreatCircle.distance c) vs
    let eDists = replicate nb r
    let distErrs = zipWith (\l1 l2 -> abs (Length.toMetres (Length.subtract l1 l2))) aDists eDists
    all (< 0.01) distErrs `shouldBe` True
    let aBrngs = mapMaybe (GreatCircle.initialBearing c) vs
    let brngErrs = zipWith (\a1 a2 -> abs (Angle.toDecimalDegrees a1 - a2)) aBrngs eBrngs
    all (< 0.01) brngErrs `shouldBe` True

simple :: (Spherical a) => [HorizontalPosition a] -> Polygon a
simple vs = fromRight (error "should be right") (Polygon.simple vs)

triangle ::
       (Spherical a)
    => HorizontalPosition a
    -> HorizontalPosition a
    -> HorizontalPosition a
    -> Triangle a
triangle = Triangle.unsafeMake
