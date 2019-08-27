module Data.Geo.Jord.ReadPositionSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

-- TODO add reading DMS + height
spec :: Spec
spec = do
    describe "Reading valid DMS text" $ do
        it "reads 553621N0130002E" $
            readPosition "553621N0130002E" WGS84 `shouldBe`
            Just (decimalLatLongPos 55.6058333 13.0005555 WGS84)
        it "reads 55°36'21''N 013°00'02''E" $
            readPosition "55°36'21''N 013°00'02''E" WGS84 `shouldBe`
            Just (decimalLatLongPos 55.6058333 13.0005555 WGS84)
        it "reads 5536N01300E" $
            readPosition "5536N01300E" WGS84 `shouldBe` Just (decimalLatLongPos 55.6 13.0 WGS84)
        it "reads 55N013E" $
            readPosition "55N013E" WGS84 `shouldBe` Just (decimalLatLongPos 55.0 13.0 WGS84)
        it "reads 011659S0364900E" $
            readPosition "011659S0364900E" WGS84 `shouldBe`
            Just (decimalLatLongPos (-1.2830555) 36.8166666 WGS84)
        it "reads 0116S03649E" $
            readPosition "0116S03649E" WGS84 `shouldBe`
            Just (decimalLatLongPos (-1.2666666) 36.8166666 WGS84)
        it "reads 1°16'S,36°49'E" $
            readPosition "1°16'S,36°49'E" WGS84 `shouldBe`
            Just (decimalLatLongPos (-1.2666666) 36.8166666 WGS84)
        it "reads 01S036E" $
            readPosition "01S036E" WGS84 `shouldBe` Just (decimalLatLongPos (-1.0) 36.0 WGS84)
        it "reads 473622N1221955W" $
            readPosition "473622N1221955W" WGS84 `shouldBe`
            Just (decimalLatLongPos 47.6061111 (-122.3319444) WGS84)
        it "reads 4736N12219W" $
            readPosition "4736N12219W" WGS84 `shouldBe`
            Just (decimalLatLongPos 47.6 (-122.3166666) WGS84)
        it "reads 47N122W" $
            readPosition "47N122W" WGS84 `shouldBe` Just (decimalLatLongPos 47.0 (-122.0) WGS84)
        it "reads 47°N 122°W" $
            readPosition "47°N 122°W" WGS84 `shouldBe` Just (decimalLatLongPos 47.0 (-122.0) WGS84)
        it "reads 544807S0681811W" $
            readPosition "544807S0681811W" WGS84 `shouldBe`
            Just (decimalLatLongPos (-54.8019444) (-68.3030555) WGS84)
        it "reads 5448S06818W" $
            readPosition "5448S06818W" WGS84 `shouldBe`
            Just (decimalLatLongPos (-54.8) (-68.3) WGS84)
        it "reads 54S068W" $
            readPosition "54S068W" WGS84 `shouldBe` Just (decimalLatLongPos (-54.0) (-68.0) WGS84)
        it "reads 54S360E (Mars)" $
            readPosition "54S360E" Mars `shouldBe` Just (decimalLatLongPos (-54.0) 360 Mars)
    describe "Reading invalid DMS text" $ do
        it "fails to read 553621K0130002E" $ readPosition "553621K0130002E" WGS84 `shouldBe` Nothing
        it "fails to read 011659S0364900Z" $ readPosition "011659S0364900Z" WGS84 `shouldBe` Nothing
        it "fails to read 4736221221955W" $ readPosition "4736221221955W" WGS84 `shouldBe` Nothing
        it "fails to read 54480S0681811W" $ readPosition "54480S0681811W" WGS84 `shouldBe` Nothing
        it "fails to read 553621N013000E" $ readPosition "553621N013000E" WGS84 `shouldBe` Nothing
        it "fails to read 914807S0681811W" $ readPosition "914807S0681811W" WGS84 `shouldBe` Nothing
        it "fails to read 544807S1811811W" $ readPosition "544807S1811811W" WGS84 `shouldBe` Nothing
        it "fails to read 546007S1801811W" $ readPosition "546007S1801811W" WGS84 `shouldBe` Nothing
        it "fails to read 545907S1801860W" $ readPosition "545907S1801860W" WGS84 `shouldBe` Nothing
        it "fails to read 5448S06818W (Mars)" $ readPosition "5448S06818W" Mars `shouldBe` Nothing