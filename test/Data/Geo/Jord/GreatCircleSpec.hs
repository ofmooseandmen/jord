module Data.Geo.Jord.GreatCircleSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec =
    describe "Jord" $ do
        describe "antipode" $ do
            it "returns the antipodal point" $
                fmap antipode (readGeoM "484137N0061105E") `geoShouldBe`
                geo (-48.69361) (-173.81527)
            it "returns the south pole when called with the north pole" $
                antipode (north :: GeoPos) `shouldBe` geo (-90.0) (-180.0)
            it "returns the north pole when called with the south pole" $
                antipode (south :: GeoPos) `shouldBe` geo 90.0 (-180.0)
        describe "distance" $ do
            it "returns the distance between 2 points" $
                distance (read "500359N0054253W" :: GeoPos) (read "583838N0030412W" :: GeoPos) `metersShouldBe`
                Meters 968854.88495
            it "handles singularity at the pole" $
                distance (north :: GeoPos) (south :: GeoPos) `metersShouldBe`
                Meters 2.001511444203e7
            it "handles the discontinuity at the Date Line" $
                distance (read "500359N1795959W" :: GeoPos) (read "500359N1795959E" :: GeoPos) `metersShouldBe`
                Meters 39.65337
        describe "destination" $ do
            it "return the estination point along great-circle at distance and bearing" $
                Just
                    (destination
                         (read "531914N0014347W" :: GeoPos)
                         (Degrees 96.0217)
                         (Meters 124800)) `geoShouldBe`
                geo 53.18826 0.13327
        describe "north pole" $ do it "returns (90, 0)" $ (north :: GeoPos) `shouldBe` geo 90.0 0.0
        describe "south pole" $ do
            it "returns (-90, 0)" $ (south :: GeoPos) `shouldBe` geo (-90.0) 0.0
