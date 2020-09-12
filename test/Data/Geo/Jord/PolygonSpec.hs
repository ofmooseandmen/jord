module Data.Geo.Jord.PolygonSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Places
import qualified Data.Geo.Jord.Polygon as Polygon

spec :: Spec
spec = do
    describe "simple" $ do
        it "returns an error if less than 3 vertices" $ do
            let ps = [Geodetic.s84Pos (-2) (-2), Geodetic.s84Pos 2 2]
            Polygon.simple ps `shouldBe` Left "not enough vertices"
        it "returns an error if edges self-intersects" $ do
            let ps =
                    [ Geodetic.s84Pos (-2) (-2)
                    , Geodetic.s84Pos 2 (-2)
                    , Geodetic.s84Pos 3 0
                    , Geodetic.s84Pos (-2) 2
                    , Geodetic.s84Pos 2 2
                    ]
            Polygon.simple ps `shouldBe` Left "self-intersecting edges"
        it "returns an error if edges self-intersects (quad)" $ do
            let ps =
                    [ Geodetic.s84Pos (-2) (-2)
                    , Geodetic.s84Pos 2 (-2)
                    , Geodetic.s84Pos (-2) 2
                    , Geodetic.s84Pos 2 2
                    ]
            Polygon.simple ps `shouldBe` Left "self-intersecting edges"
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
