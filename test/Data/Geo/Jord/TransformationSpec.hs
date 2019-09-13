module Data.Geo.Jord.TransformationSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "coordinates static transformation" $ do
        it "returns the initial coordinates if all parameters are 0" $ do
            let tx7 = txParams7 (0, 0, 0) 0 (0, 0, 0)
            let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
            transformCoords pWGS84 WGS84 tx7 `shouldBe` pWGS84
        it "uses the 7-parameter transformation" $ do
            let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
            let tx = txParams from_WGS84_to_NAD83
            let pNAD83 = transformCoords pWGS84 NAD83 tx
            pNAD83 `shouldBe`
                latLongHeightPos
                    48.69208978369768
                    6.184367561060834
                    (metres 188.12122946884483)
                    NAD83
        it "returns the initial coordinates when doing round-trip (direct -> inverse)" $ do
            let tx = txParams from_WGS84_to_NAD83
            let itx = inverseTxParams from_WGS84_to_NAD83
            let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
            transformCoords (transformCoords pWGS84 NAD83 tx) WGS84 itx `shouldBe` pWGS84
    describe "coordinates dynamic transformation" $ do
        it "returns the initial coordinates if all parameters are 0" $ do
            let tx15 =
                    TxParams15
                        (Epoch 2010)
                        (txParams7 (0, 0, 0) 0 (0, 0, 0))
                        (txRates (0, 0, 0) 0 (0, 0, 0))
            let pWGS84 = latLongHeightPos 48.6921 6.1844 (metres 188) WGS84_G1762
            transformCoordsAt pWGS84 (Epoch 2010.0) WGS84_G1762 tx15 `shouldBe` pWGS84
        it "uses the 15-parameter transformation and position epoch" $ do
            let pITRF2014 = geocentricMetresPos 4027894.006 307045.600 4919474.910 ITRF2014
            let tx = txParams from_ITRF2014_to_ETRF2000
            let pETRF2000 = transformCoordsAt pITRF2014 (Epoch 2012.0) ETRF2000 tx
            pETRF2000 `shouldBe` geocentricMetresPos 4027894.0452 307045.876 4919474.8735 ETRF2000
        it "returns the initial coordinates when doing round-trip (direct -> inverse)" $ do
            let tx = txParams from_ITRF2014_to_ETRF2000
            let itx = inverseTxParams from_ITRF2014_to_ETRF2000
            let pITRF2014 = geocentricMetresPos 4027894.006 307045.600 4919474.910 ITRF2014
            let e = Epoch 2019.0
            transformCoordsAt (transformCoordsAt pITRF2014 e ETRF2000 tx) e ITRF2014 itx `shouldBe`
                pITRF2014
