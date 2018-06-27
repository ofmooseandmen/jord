module Data.Geo.JordParsingSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

-- | Tests the parsing of GeoPos.
spec :: Spec
spec =
    describe "GeoPos parser" $ do
        it "parses 553621N0130002E" $
            (read "553621N0130002E" :: GeoPos)
            `shouldBe`
            geo 55.60583333333334 13.000555555555556
        it "parses 5536N01300E" $
            (read "5536N01300E" :: GeoPos)
            `shouldBe`
            geo 55.6 13.0
        it "parses 55N013E" $
            (read "55N013E" :: GeoPos)
            `shouldBe`
            geo 55.0 13.0
        it "parses 011659S0364900E" $
            (read "011659S0364900E" :: GeoPos)
            `shouldBe`
            geo (-1.2830555555555556) 36.81666666666667
        it "parses 0116S03649E" $
            (read "0116S03649E" :: GeoPos)
            `shouldBe`
            geo (-1.2666666666666666) 36.81666666666667
        it "parses 01S036E" $
            (read "01S036E" :: GeoPos)
            `shouldBe`
            geo (-1.0) 36.0
        it "parses 473622N1221955W" $
            (read "473622N1221955W" :: GeoPos)
            `shouldBe`
            geo 47.60611111111111 (-122.33194444444445)
        it "parses 4736N12219W" $
            (read "4736N12219W" :: GeoPos)
            `shouldBe`
            geo 47.6 (-122.31666666666666)
        it "parses 47N122W" $
            (read "47N122W" :: GeoPos)
            `shouldBe`
            geo 47.0 (-122.0)
        it "parses 544807S0681811W" $
            (read "544807S0681811W" :: GeoPos)
            `shouldBe`
            geo (-54.801944444444445) (-68.30305555555556)
        it "parses 5448S06818W" $
            (read "5448S06818W" :: GeoPos)
            `shouldBe`
            geo (-54.8) (-68.3)
        it "parses 54S068W" $
            (read "54S068W" :: GeoPos)
            `shouldBe`
            geo (-54.0) (-68.0)
