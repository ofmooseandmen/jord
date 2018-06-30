module Data.Geo.JordParsingSpec
    ( spec
    ) where

import Data.Geo.Expectations
import Data.Geo.Jord
import Text.Read
import Test.Hspec

-- | Tests parsing GeoPos.
spec :: Spec
spec =
    describe "GeoPos parser" $ do
        describe "Parsing valid DMS text" $ do
            it "parses 553621N0130002E" $
                (read "553621N0130002E" :: GeoPos)
                `geoShouldBe`
                geo 55.60583 13.00055
            it "parses 55°36'21''N 013°00'02''E" $
                (read "55°36'21''N 013°00'02''E" :: GeoPos)
                `geoShouldBe`
                geo 55.60583 13.00055
            it "parses 5536N01300E" $
                (read "5536N01300E" :: GeoPos)
                `geoShouldBe`
                geo 55.6 13.0
            it "parses 55N013E" $
                (read "55N013E" :: GeoPos)
                `geoShouldBe`
                geo 55.0 13.0
            it "parses 011659S0364900E" $
                (read "011659S0364900E" :: GeoPos)
                `geoShouldBe`
                geo (-1.28305) 36.81666
            it "parses 0116S03649E" $
                (read "0116S03649E" :: GeoPos)
                `geoShouldBe`
                geo (-1.26666) 36.81666
            it "parses 01S036E" $
                (read "01S036E" :: GeoPos)
                `geoShouldBe`
                geo (-1.0) 36.0
            it "parses 473622N1221955W" $
                (read "473622N1221955W" :: GeoPos)
                `geoShouldBe`
                geo 47.60611 (-122.33194)
            it "parses 4736N12219W" $
                (read "4736N12219W" :: GeoPos)
                `geoShouldBe`
                geo 47.6 (-122.31666)
            it "parses 47N122W" $
                (read "47N122W" :: GeoPos)
                `geoShouldBe`
                geo 47.0 (-122.0)
            it "parses 544807S0681811W" $
                (read "544807S0681811W" :: GeoPos)
                `geoShouldBe`
                geo (-54.80194) (-68.30305)
            it "parses 5448S06818W" $
                (read "5448S06818W" :: GeoPos)
                `geoShouldBe`
                geo (-54.8) (-68.3)
            it "parses 54S068W" $
                (read "54S068W" :: GeoPos)
                `geoShouldBe`
                geo (-54.0) (-68.0)
        describe "Parsing invalid DMS text" $ do
            it "fails to parse 553621K0130002E" $
                (readMaybe "553621K0130002E" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 011659S0364900Z" $
                (readMaybe "011659S0364900Z" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 4736221221955W" $
                (readMaybe "4736221221955W" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 54480S0681811W" $
                (readMaybe "54480S0681811W" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 553621N013000E" $
                (readMaybe "553621N013000E" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 914807S0681811W" $
                (readMaybe "914807S0681811W" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 544807S1811811W" $
                (readMaybe "544807S1811811W" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 546007S1801811W" $
                (readMaybe "546007S1801811W" :: Maybe GeoPos)
                `shouldBe` Nothing
            it "fails to parse 545907S1801860W" $
                (readMaybe "545907S1801860W" :: Maybe GeoPos)
                `shouldBe` Nothing
