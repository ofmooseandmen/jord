module Data.Geo.Jord.TransformationSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec =
    describe "coordinates static transformation" $ do
        it "returns the initial coordinates all parameters are 0" $ do
            let tx7 = txParams7 (0, 0, 0) 0 (0, 0, 0)
            let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
            transformCoords pWGS84 WGS84 tx7 `shouldBe` pWGS84
        it "uses the 7-parametres transformation" $ do
            let tx7 = txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599) -- WGS84 -> NAD83
            let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
            let pNAD83 = transformCoords pWGS84 NAD83 tx7
            pNAD83 `shouldBe` latLongHeightPos 48.69208978369768 6.184367561060834 (metres 188.12122946884483) NAD83
        it "returns the initial coordinates when doing round-trip (direct -> inverse)" $ do
            let tx7 = txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599) -- WGS84 -> NAD83
            let itx7 = inverseTxParams7 tx7
            let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
            transformCoords (transformCoords pWGS84 NAD83 tx7) WGS84 itx7 `shouldBe` pWGS84