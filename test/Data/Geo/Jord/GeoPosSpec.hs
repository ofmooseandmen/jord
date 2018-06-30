module Data.Geo.Jord.GeoPosSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec =
    describe "Read GeoPos" $ do
        describe "Reading valid DMS text" $ do
            it "reads 553621N0130002E" $
                readGeo "553621N0130002E" `geoShouldBe` geo 55.60583 13.00055
            it "reads 55°36'21''N 013°00'02''E" $
                readGeo "55°36'21''N 013°00'02''E" `geoShouldBe` geo 55.60583 13.00055
            it "reads 5536N01300E" $ readGeo "5536N01300E" `geoShouldBe` geo 55.6 13.0
            it "reads 55N013E" $ readGeo "55N013E" `geoShouldBe` geo 55.0 13.0
            it "reads 011659S0364900E" $
                readGeo "011659S0364900E" `geoShouldBe` geo (-1.28305) 36.81666
            it "reads 0116S03649E" $ readGeo "0116S03649E" `geoShouldBe` geo (-1.26666) 36.81666
            it "reads 01S036E" $ readGeo "01S036E" `geoShouldBe` geo (-1.0) 36.0
            it "reads 473622N1221955W" $
                readGeo "473622N1221955W" `geoShouldBe` geo 47.60611 (-122.33194)
            it "reads 4736N12219W" $ readGeo "4736N12219W" `geoShouldBe` geo 47.6 (-122.31666)
            it "reads 47N122W" $ readGeo "47N122W" `geoShouldBe` geo 47.0 (-122.0)
            it "reads 544807S0681811W" $
                readGeo "544807S0681811W" `geoShouldBe` geo (-54.80194) (-68.30305)
            it "reads 5448S06818W" $ readGeo "5448S06818W" `geoShouldBe` geo (-54.8) (-68.3)
            it "reads 54S068W" $ readGeo "54S068W" `geoShouldBe` geo (-54.0) (-68.0)
        describe "Reading invalid DMS text" $ do
            it "fails to read 553621K0130002E" $ readGeoM "553621K0130002E" `shouldBe` Nothing
            it "fails to read 011659S0364900Z" $ readGeoM "011659S0364900Z" `shouldBe` Nothing
            it "fails to read 4736221221955W" $ readGeoM "4736221221955W" `shouldBe` Nothing
            it "fails to read 54480S0681811W" $ readGeoM "54480S0681811W" `shouldBe` Nothing
            it "fails to read 553621N013000E" $ readGeoM "553621N013000E" `shouldBe` Nothing
            it "fails to read 914807S0681811W" $ readGeoM "914807S0681811W" `shouldBe` Nothing
            it "fails to read 544807S1811811W" $ readGeoM "544807S1811811W" `shouldBe` Nothing
            it "fails to read 546007S1801811W" $ readGeoM "546007S1801811W" `shouldBe` Nothing
            it "fails to read 545907S1801860W" $ readGeoM "545907S1801860W" `shouldBe` Nothing
