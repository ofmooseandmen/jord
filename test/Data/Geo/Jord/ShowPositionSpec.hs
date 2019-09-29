module Data.Geo.Jord.ShowPositionSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord.Position

spec :: Spec
spec =
    describe "Showing positions" $ do
        it "shows the N/E position formatted in DMS with symbols" $
            show (wgs84Pos 55.6058333333 13.00055556 (metres 5)) `shouldBe`
            "55°36'21.000\"N,13°0'2.000\"E 5.0m (WGS84)"
        it "shows the S/E position formatted in DMS with symbols" $
            show (latLongPos (-1.28305556) 36.81666 GRS80) `shouldBe`
            "1°16'59.000\"S,36°48'59.976\"E 0.0m (GRS80)"
        it "shows the N/W position formatted in DMS with symbols" $
            show (latLongPos 47.60611 (-122.33194) S84) `shouldBe`
            "47°36'21.996\"N,122°19'54.984\"W 0.0m (S84)"
        it "shows the S/W position formatted in DMS with symbols" $
            show (latLongPos (-54.80194) (-68.30305) S84) `shouldBe`
            "54°48'6.984\"S,68°18'10.980\"W 0.0m (S84)"
