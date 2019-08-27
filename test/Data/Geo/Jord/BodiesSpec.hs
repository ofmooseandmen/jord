module Data.Geo.Jord.BodiesSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "Eccentricity" $ do
        it "returns 0.08181919084262157 for the WGS84 ellipsoid" $
            modelEccentricity WGS84 `shouldBe` 0.08181919084262157
        it "returns 0.08181919104281514 for the GRS80 ellipsoid" $
            modelEccentricity GRS80 `shouldBe` 0.08181919104281514
        it "returns 0.08181881066274845 for the WG72 ellipsoid" $
            modelEccentricity WGS72 `shouldBe` 0.08181881066274845
        it "returns 0 for the spherical models" $ do
            modelEccentricity S84 `shouldBe` 0
            modelEccentricity S80 `shouldBe` 0
            modelEccentricity S72 `shouldBe` 0
    describe "Polar radius" $ do
        it "returns 6356752.3142 m for the WGS84 ellipsoid" $
            modelPolarRadius WGS84 `shouldBe` metres 6356752.3142
        it "returns 6356752.3141 m for the GRS80 ellipsoid" $
            modelPolarRadius GRS80 `shouldBe` metres 6356752.3141
        it "returns 6356750.52 m for the WG72 ellipsoid" $
            modelPolarRadius WGS72 `shouldBe` metres 6356750.52
    describe "Mean radius" $ do
        it "returns 6371008.7714 m for the WGS84 ellipsoid" $
            modelRadius S84 `shouldBe` metres 6371008.7714
        it "returns 6371008.7714 m for the GRS80 ellipsoid" $
            modelRadius S80 `shouldBe` metres 6371008.7714
        it "returns 6371006.84 m for the WG72 ellipsoid" $
            modelRadius S72 `shouldBe` metres 6371006.84