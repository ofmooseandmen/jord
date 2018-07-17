module Data.Geo.Jord.EllipsoidSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Eccentricity" $
        it "returns 0.08181919084262157 for the WGS84 ellipsoid" $
        eccentricity wgs84 `shouldBe` 0.08181919084262157
    describe "Polar radius" $
        it "returns 6356752.314 m for the WGS84 ellipsoid" $
        polarRadius wgs84 `shouldBe` metres 6356752.314
    describe "Mean radius" $
        it "returns 6371008.771 m for the WGS84 ellipsoid" $
        meanRadius wgs84 `shouldBe` metres 6371008.771
