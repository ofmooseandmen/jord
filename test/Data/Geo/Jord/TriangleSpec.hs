module Data.Geo.Jord.TriangleSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import Data.Geo.Jord.Places
import qualified Data.Geo.Jord.Triangle as Triangle

spec :: Spec
spec = do
    describe "make" $ do
        it "returns Nothing if any 2 positions are equal" $ do
            let v0 = Geodetic.s84Pos 0 0
            Triangle.make v0 v0 (Geodetic.s84Pos 10 0) `shouldBe` Nothing
            Triangle.make v0 (Geodetic.s84Pos 10 0) v0 `shouldBe` Nothing
            Triangle.make (Geodetic.s84Pos 10 0) v0 v0 `shouldBe` Nothing
        it "returns Nothing if any position is the antipode of another one" $ do
            let v0 = Geodetic.s84Pos 0 0
            Triangle.make v0 (Geodetic.antipode v0) (Geodetic.s84Pos 10 0) `shouldBe` Nothing
            Triangle.make v0 (Geodetic.s84Pos 10 0) (Geodetic.antipode v0) `shouldBe` Nothing
            Triangle.make (Geodetic.s84Pos 10 0) v0 (Geodetic.antipode v0) `shouldBe` Nothing
    describe "centroid" $ do
        it "returns the intersection of the medians" $ do
            let t =
                    Triangle.unsafeMake
                        (Geodetic.s84Pos 0 (-10))
                        (Geodetic.s84Pos 10 0)
                        (Geodetic.s84Pos 0 10)
            let c = Triangle.centroid t
            c `shouldBe` Geodetic.s84Pos 3.3637274116666664 0
            Triangle.contains t c `shouldBe` True
    describe "circumcentre" $ do
        it "returns the position equidistant from all vertices" $ do
            let t = Triangle.unsafeMake malmo stockholm goteborg
            let c = Triangle.circumcentre t
            let d1 = GreatCircle.distance c malmo
            let d2 = GreatCircle.distance c stockholm
            let d3 = GreatCircle.distance c goteborg
            d1 `shouldBe` d2
            d1 `shouldBe` d3
            d2 `shouldBe` d3
