module Data.Geo.Jord.TransformationSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Geocentric as Geocentric
import Data.Geo.Jord.Model (Epoch(..))
import Data.Geo.Jord.Models
import Data.Geo.Jord.Transformation

spec :: Spec
spec = do
    describe "coordinates static transformation" $ do
        it "returns the initial coordinates if all parameters are 0" $ do
            let tx7 = txParams7 (0, 0, 0) 0 (0, 0, 0)
            let pWGS84 = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84
            transformCoords' pWGS84 WGS84 tx7 `shouldBe` pWGS84
        it "uses the 7-parameter transformation" $ do
            let pWGS84 = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84
            let tx = txParams from_WGS84_to_NAD83
            let pNAD83 = transformCoords' pWGS84 NAD83 tx
            pNAD83 `shouldBe` Geocentric.metresPos 4193792.080781 454433.921298 4768166.15479 NAD83
        it "returns the initial coordinates when doing round-trip (direct -> inverse)" $ do
            let tx = txParams from_WGS84_to_ETRS89
            let itx = inverseTxParams tx
            let pWGS84 = Geocentric.metresPos 3194419.145061 3194419.145061 4487348.408866 WGS84
            transformCoords' (transformCoords' pWGS84 NAD83 tx) WGS84 itx `shouldBe` pWGS84
    describe "coordinates dynamic transformation" $ do
        it "returns the initial coordinates if all parameters are 0" $ do
            let tx15 =
                    TxParams15
                        (Epoch 2010)
                        (txParams7 (0, 0, 0) 0 (0, 0, 0))
                        (txRates (0, 0, 0) 0 (0, 0, 0))
            let pWGS84 = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84_G1762
            transformCoordsAt' pWGS84 (Epoch 2010.0) WGS84_G1762 tx15 `shouldBe` pWGS84
        it "uses the 15-parameter transformation and position epoch" $ do
            let pITRF2014 = Geocentric.metresPos 4027894.006 307045.600 4919474.910 ITRF2014
            let tx = txParams from_ITRF2014_to_ETRF2000
            let pETRF2000 = transformCoordsAt' pITRF2014 (Epoch 2012.0) ETRF2000 tx
            pETRF2000 `shouldBe`
                Geocentric.metresPos 4027894.366234 307045.252967 4919474.626307 ETRF2000
        it "returns the initial coordinates when doing round-trip (direct -> inverse)" $ do
            let tx = txParams from_ITRF2014_to_ETRF2000
            let itx = inverseTxParams tx
            let pITRF2014 = Geocentric.metresPos 4027894.006 307045.600 4919474.910 ITRF2014
            let e = Epoch 2019.0
            transformCoordsAt' (transformCoordsAt' pITRF2014 e ETRF2000 tx) e ITRF2014 itx `shouldBe`
                pITRF2014
