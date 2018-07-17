module Data.Geo.Jord.EllipsoidSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Polar radius" $
        it "returns 6356752.314 m for the WGS84 ellipsoid" $
        polarRadius wgs84 `shouldBe` metres 6356752.314
    describe "Mean radius" $
        it "returns 6371008.771 m for the WGS84 ellipsoid" $
        meanRadius wgs84 `shouldBe` metres 6371008.771
