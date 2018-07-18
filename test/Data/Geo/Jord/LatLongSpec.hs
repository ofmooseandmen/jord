module Data.Geo.Jord.LatLongSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid DMS text" $ do
        it "reads 553621N0130002E" $
            readLatLong "553621N0130002E" `shouldBe` latLongDecimal 55.6058333 13.0005555
        it "reads 55°36'21''N 013°00'02''E" $
            readLatLong "55°36'21''N 013°00'02''E" `shouldBe` latLongDecimal 55.6058333 13.0005555
        it "reads 5536N01300E" $ readLatLong "5536N01300E" `shouldBe` latLongDecimal 55.6 13.0
        it "reads 55N013E" $ readLatLong "55N013E" `shouldBe` latLongDecimal 55.0 13.0
        it "reads 011659S0364900E" $
            readLatLong "011659S0364900E" `shouldBe` latLongDecimal (-1.2830555) 36.8166666
        it "reads 0116S03649E" $
            readLatLong "0116S03649E" `shouldBe` latLongDecimal (-1.2666666) 36.8166666
        it "reads 1°16'S,36°49'E" $
            readLatLong "1°16'S,36°49'E" `shouldBe` latLongDecimal (-1.2666666) 36.8166666
        it "reads 01S036E" $ readLatLong "01S036E" `shouldBe` latLongDecimal (-1.0) 36.0
        it "reads 473622N1221955W" $
            readLatLong "473622N1221955W" `shouldBe` latLongDecimal 47.6061111 (-122.3319444)
        it "reads 4736N12219W" $
            readLatLong "4736N12219W" `shouldBe` latLongDecimal 47.6 (-122.3166666)
        it "reads 47N122W" $ readLatLong "47N122W" `shouldBe` latLongDecimal 47.0 (-122.0)
        it "reads 47°N 122°W" $ readLatLong "47°N 122°W" `shouldBe` latLongDecimal 47.0 (-122.0)
        it "reads 544807S0681811W" $
            readLatLong "544807S0681811W" `shouldBe` latLongDecimal (-54.8019444) (-68.3030555)
        it "reads 5448S06818W" $ readLatLong "5448S06818W" `shouldBe` latLongDecimal (-54.8) (-68.3)
        it "reads 54S068W" $ readLatLong "54S068W" `shouldBe` latLongDecimal (-54.0) (-68.0)
    describe "Reading invalid DMS text" $ do
        it "fails to read 553621K0130002E" $
            readLatLongE "553621K0130002E" `shouldBe` Left "couldn't read geo pos 553621K0130002E"
        it "fails to read 011659S0364900Z" $
            readLatLongE "011659S0364900Z" `shouldBe` Left "couldn't read geo pos 011659S0364900Z"
        it "fails to read 4736221221955W" $
            readLatLongE "4736221221955W" `shouldBe` Left "couldn't read geo pos 4736221221955W"
        it "fails to read 54480S0681811W" $
            readLatLongE "54480S0681811W" `shouldBe` Left "couldn't read geo pos 54480S0681811W"
        it "fails to read 553621N013000E" $
            readLatLongE "553621N013000E" `shouldBe` Left "couldn't read geo pos 553621N013000E"
        it "fails to read 914807S0681811W" $
            readLatLongE "914807S0681811W" `shouldBe` Left "couldn't read geo pos 914807S0681811W"
        it "fails to read 544807S1811811W" $
            readLatLongE "544807S1811811W" `shouldBe` Left "couldn't read geo pos 544807S1811811W"
        it "fails to read 546007S1801811W" $
            readLatLongE "546007S1801811W" `shouldBe` Left "couldn't read geo pos 546007S1801811W"
        it "fails to read 545907S1801860W" $
            readLatLongE "545907S1801860W" `shouldBe` Left "couldn't read geo pos 545907S1801860W"
    describe "Showing geographic positions" $ do
        it "shows the N/E position formatted in DMS with symbols" $
            show (latLongDecimal 55.60583333 13.00055556) `shouldBe` "55°36'21.000\"N,13°0'2.000\"E"
        it "shows the S/E position formatted in DMS with symbols" $
            show (latLongDecimal (-1.28305556) 36.81666) `shouldBe` "1°16'59.000\"S,36°48'59.976\"E"
        it "shows the N/W position formatted in DMS with symbols" $
            show (latLongDecimal 47.60611 (-122.33194)) `shouldBe`
            "47°36'21.996\"N,122°19'54.984\"W"
        it "shows the S/W position formatted in DMS with symbols" $
            show (latLongDecimal (-54.80194) (-68.30305)) `shouldBe`
            "54°48'6.984\"S,68°18'10.980\"W"
