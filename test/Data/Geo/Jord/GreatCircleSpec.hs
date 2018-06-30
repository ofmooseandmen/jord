module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec =
    describe "GreatCircle" $ do
        describe "antipode" $ do
            it "returns the antipodal point" $
                antipode (readGeo "484137N0061105E") `geoShouldBe` geo (-48.69361) (-173.81527)
            it "returns the south pole when called with the north pole" $
                antipode (northPole :: GeoPos) `shouldBe` geo (-90.0) (-180.0)
            it "returns the north pole when called with the south pole" $
                antipode (southPole :: GeoPos) `shouldBe` geo 90.0 (-180.0)
        describe "distance" $ do
            it "returns the distance between 2 points" $
                distance (readGeo "500359N0054253W") (readGeo "583838N0030412W") `metersShouldBe`
                Meters 968854.88495
            it "handles singularity at the pole" $
                distance (northPole :: GeoPos) (southPole :: GeoPos) `metersShouldBe`
                Meters 2.001511444203e7
            it "handles the discontinuity at the Date Line" $
                distance (readGeo "500359N1795959W") (readGeo "500359N1795959E") `metersShouldBe`
                Meters 39.65337
            it "returns 0 if both points are equal" $
                distance (readGeo "500359N1795959W") (readGeo "500359N1795959W") `metersShouldBe`
                Meters 0.0
        describe "destination" $ do
            it "return the estination point along great-circle at distance and bearing" $
                destination (readGeo "531914N0014347W") (Degrees 96.0217) (Meters 124800) `geoShouldBe`
                geo 53.18826 0.13327
        describe "north pole" $ it "returns (90, 0)" $ (northPole :: GeoPos) `shouldBe` geo 90.0 0.0
        describe "south pole" $
            it "returns (-90, 0)" $ (southPole :: GeoPos) `shouldBe` geo (-90.0) 0.0
