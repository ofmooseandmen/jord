module Data.Geo.Jord.EarthSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Eccentricity" $ do
        it "returns 0.08181919084262157 for the WGS84 ellipsoid" $
          eccentricity wgs84 `shouldBe` 0.08181919084262157
        it "returns 0.08181919104281514 for the GRS80 ellipsoid" $
          eccentricity grs80 `shouldBe` 0.08181919104281514
        it "returns 0.08181881066274845 for the WG72 ellipsoid" $
          eccentricity wgs72 `shouldBe` 0.08181881066274845
    describe "Polar radius" $ do
        it "returns 6356752.314 m for the WGS84 ellipsoid" $
          polarRadius wgs84 `shouldBe` metres 6356752.314
        it "returns 6356752.314 m for the GRS80 ellipsoid" $
          polarRadius grs80 `shouldBe` metres 6356752.314
        it "returns 6356750.52 m for the WG72 ellipsoid" $
          polarRadius wgs72 `shouldBe` metres 6356750.52
    describe "Mean radius" $ do
        it "returns 6371008.771 m for the WGS84 ellipsoid" $
          r84 `shouldBe` metres 6371008.771
        it "returns 6371008.771 m for the GRS80 ellipsoid" $
          r80 `shouldBe` metres 6371008.771
        it "returns 6371006.84 m for the WG72 ellipsoid" $
          r72 `shouldBe` metres 6371006.84
