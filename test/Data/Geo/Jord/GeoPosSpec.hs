module Data.Geo.Jord.GeoPosSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid DMS text" $ do
        it "reads 553621N0130002E" $
            readGeoPos "553621N0130002E" `geoShouldBe` geoPos 55.6058333 13.0005555
        it "reads 55°36'21''N 013°00'02''E" $
            readGeoPos "55°36'21''N 013°00'02''E" `geoShouldBe` geoPos 55.6058333 13.0005555
        it "reads 5536N01300E" $ readGeoPos "5536N01300E" `geoShouldBe` geoPos 55.6 13.0
        it "reads 55N013E" $ readGeoPos "55N013E" `geoShouldBe` geoPos 55.0 13.0
        it "reads 011659S0364900E" $
            readGeoPos "011659S0364900E" `geoShouldBe` geoPos (-1.2830555) 36.8166666
        it "reads 0116S03649E" $
            readGeoPos "0116S03649E" `geoShouldBe` geoPos (-1.2666666) 36.8166666
        it "reads 1°16'S 36°49'E" $
            readGeoPos "1°16'S 36°49'E" `geoShouldBe` geoPos (-1.2666666) 36.8166666
        it "reads 01S036E" $ readGeoPos "01S036E" `geoShouldBe` geoPos (-1.0) 36.0
        it "reads 473622N1221955W" $
            readGeoPos "473622N1221955W" `geoShouldBe` geoPos 47.6061111 (-122.3319444)
        it "reads 4736N12219W" $ readGeoPos "4736N12219W" `geoShouldBe` geoPos 47.6 (-122.3166666)
        it "reads 47N122W" $ readGeoPos "47N122W" `geoShouldBe` geoPos 47.0 (-122.0)
        it "reads 47°N 122°W" $ readGeoPos "47°N 122°W" `geoShouldBe` geoPos 47.0 (-122.0)
        it "reads 544807S0681811W" $
            readGeoPos "544807S0681811W" `geoShouldBe` geoPos (-54.8019444) (-68.3030555)
        it "reads 5448S06818W" $ readGeoPos "5448S06818W" `geoShouldBe` geoPos (-54.8) (-68.3)
        it "reads 54S068W" $ readGeoPos "54S068W" `geoShouldBe` geoPos (-54.0) (-68.0)
    describe "Reading invalid DMS text" $ do
        it "fails to read 553621K0130002E" $ readGeoPosM "553621K0130002E" `shouldBe` Nothing
        it "fails to read 011659S0364900Z" $ readGeoPosM "011659S0364900Z" `shouldBe` Nothing
        it "fails to read 4736221221955W" $ readGeoPosM "4736221221955W" `shouldBe` Nothing
        it "fails to read 54480S0681811W" $ readGeoPosM "54480S0681811W" `shouldBe` Nothing
        it "fails to read 553621N013000E" $ readGeoPosM "553621N013000E" `shouldBe` Nothing
        it "fails to read 914807S0681811W" $ readGeoPosM "914807S0681811W" `shouldBe` Nothing
        it "fails to read 544807S1811811W" $ readGeoPosM "544807S1811811W" `shouldBe` Nothing
        it "fails to read 546007S1801811W" $ readGeoPosM "546007S1801811W" `shouldBe` Nothing
        it "fails to read 545907S1801860W" $ readGeoPosM "545907S1801860W" `shouldBe` Nothing
    describe "Showing GeoPos" $ do
        it "return the N/E position formatted in DMS with symbols" $
            show (geoPos 55.60583333 13.00055556) `shouldBe` "55°36'21.0\"N 13°0'2.0\"E"
        it "return the S/E position formatted in DMS with symbols" $
            show (geoPos (-1.28305556) 36.81666) `shouldBe` "1°16'59.0\"S 36°48'59.976\"E"
        it "return the N/W position formatted in DMS with symbols" $
            show (geoPos 47.60611 (-122.33194)) `shouldBe` "47°36'21.996\"N 122°19'54.984\"W"
        it "return the S/W position formatted in DMS with symbols" $
            show (geoPos (-54.80194) (-68.30305)) `shouldBe` "54°48'6.984\"S 68°18'10.98\"W"
