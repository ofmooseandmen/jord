module Data.Geo.Jord.EllipsoidSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "Eccentricity" $ do
        it "returns 0.08181919084262157 for the WGS84 ellipsoid" $
            eccentricity eWGS84 `shouldBe` 0.08181919084262157
        it "returns 0.08181919104281514 for the GRS80 ellipsoid" $
            eccentricity eGRS80 `shouldBe` 0.08181919104281514
        it "returns 0.08181881066274845 for the WG72 ellipsoid" $
            eccentricity eWGS72 `shouldBe` 0.08181881066274845
        it "returns 0 for the spherical models" $ do
            eccentricity (toSphere eWGS84) `shouldBe` 0
            eccentricity (toSphere eGRS80) `shouldBe` 0
            eccentricity (toSphere eWGS72) `shouldBe` 0
    describe "Polar radius" $ do
        it "returns 6356752.3142 m for the WGS84 ellipsoid" $
            polarRadius eWGS84 `shouldBe` metres 6356752.3142
        it "returns 6356752.3141 m for the GRS80 ellipsoid" $
            polarRadius eGRS80 `shouldBe` metres 6356752.3141
        it "returns 6356750.52 m for the WG72 ellipsoid" $
            polarRadius eWGS72 `shouldBe` metres 6356750.52
    describe "Mean radius" $ do
        it "returns 6371008.7714 m for the WGS84 ellipsoid" $
            meanRadius eWGS84 `shouldBe` metres 6371008.7714
        it "returns 6371008.7714 m for the GRS80 ellipsoid" $
            meanRadius eGRS80 `shouldBe` metres 6371008.7714
        it "returns 6371006.84 m for the WG72 ellipsoid" $
            meanRadius eWGS72 `shouldBe` metres 6371006.84