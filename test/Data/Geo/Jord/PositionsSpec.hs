module Data.Geo.Jord.PositionsSpec
    ( spec
    ) where

import Test.Hspec

import Data.Maybe (fromJust)

import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Model (Epoch(..))
import Data.Geo.Jord.Models
import qualified Data.Geo.Jord.Positions as Positions
import qualified Data.Geo.Jord.Tx as Tx
import qualified Data.Geo.Jord.Txs as Txs

spec :: Spec
spec = do
    describe "Geodetic <=> Geocentric (Ellipsoidal)" $ do
        it "n-vector <=> Geocentric" $ do
            let p = Geodetic.nvectorHeightPos 0.5 0.5 0.7071067811865475 Length.zero WGS84
            let g = Geocentric.metresPos 3194419.145061 3194419.145061 4487348.408866 WGS84
            Positions.toGeocentric p `shouldBe` g
            Positions.toGeodetic g `shouldBe` p
        it "latitude, longitude and height <=> Geocentric" $ do
            let refLlh =
                    [ Geodetic.latLongHeightPos 0 0 Length.zero WGS84
                    , Geodetic.latLongHeightPos 90 0 Length.zero WGS84
                    , Geodetic.latLongHeightPos (-90) 0 Length.zero WGS84
                    , Geodetic.latLongHeightPos 45.0 45.0 (Length.metres 500) WGS84
                    , Geodetic.latLongHeightPos (-45) (-45) (Length.metres 500) WGS84
                    ]
            let refGeocentrics =
                    [ Geocentric.metresPos 6378137 0 0 WGS84
                    , Geocentric.metresPos 0 0 6356752.314245 WGS84
                    , Geocentric.metresPos 0 0 (-6356752.314245) WGS84
                    , Geocentric.metresPos 3194669.145061 3194669.145061 4487701.962256 WGS84
                    , Geocentric.metresPos 3194669.145061 (-3194669.145061) (-4487701.962256) WGS84
                    ]
            fmap Positions.toGeocentric refLlh `shouldBe` refGeocentrics
            fmap Positions.toGeodetic refGeocentrics `shouldBe` refLlh
    describe "Geodetic <=> Geocentric (Spherical)" $ do
        it "n-vector <=> Geocentric" $ do
            let p = Geodetic.nvectorHeightPos 0.5 0.5 0.7071 Length.zero S84
            let g = Geocentric.metresPos 3185519.660307 3185519.660307 4504961.903617 S84
            Positions.toGeocentric p `shouldBe` g
            Positions.toGeodetic g `shouldBe` p
        it "latitude, longitude and height <=> Geocentric" $ do
            let refLlh =
                    [ Geodetic.latLongHeightPos 0 0 Length.zero S84
                    , Geodetic.latLongHeightPos 90 0 Length.zero S84
                    , Geodetic.latLongHeightPos (-90) 0 Length.zero S84
                    , Geodetic.latLongHeightPos 45.0 45.0 (Length.metres 500) S84
                    , Geodetic.latLongHeightPos (-45) (-45) (Length.metres 500) S84
                    ]
            let refGeocentrics =
                    [ Geocentric.metresPos 6371008.771415 0 0 S84
                    , Geocentric.metresPos 0 0 6371008.771415 S84
                    , Geocentric.metresPos 0 0 (-6371008.771415) S84
                    , Geocentric.metresPos 3185754.385708 3185754.385708 4505337.058657 S84
                    , Geocentric.metresPos 3185754.385708 (-3185754.385708) (-4505337.058657) S84
                    ]
            fmap Positions.toGeocentric refLlh `shouldBe` refGeocentrics
            fmap Positions.toGeodetic refGeocentrics `shouldBe` refLlh
    describe "coordinates transformation - fixed" $ do
        it "returns the initial coordinates if all parameters are 0" $ do
            let tx7 = Tx.params7 (0, 0, 0) 0 (0, 0, 0)
            let pWGS84 = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84
            Positions.transform' pWGS84 WGS84 tx7 `shouldBe` pWGS84
        it "uses the 7-parameter transformation" $ do
            let pWGS84 = Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84
            let tx = Tx.params Txs.from_WGS84_to_NAD83
            let pNAD83 = Positions.transform' pWGS84 NAD83 tx
            pNAD83 `shouldBe` Geocentric.metresPos 4193792.080781 454433.921298 4768166.15479 NAD83
        it "returns the initial coordinates when doing round-trip (direct -> inverse)" $ do
            let tx = Tx.params Txs.from_WGS84_to_ETRS89
            let itx = Tx.inverseParams tx
            let pWGS84 = Geocentric.metresPos 3194419.145061 3194419.145061 4487348.408866 WGS84
            Positions.transform' (Positions.transform' pWGS84 NAD83 tx) WGS84 itx `shouldBe` pWGS84
    describe "coordinates transformation - time dependent" $ do
        it "returns the initial coordinates if all parameters are 0" $ do
            let tx15 =
                    Tx.Params15
                        (Epoch 2010)
                        (Tx.params7 (0, 0, 0) 0 (0, 0, 0))
                        (Tx.rates (0, 0, 0) 0 (0, 0, 0))
            let pWGS84 =
                    Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84_G1762
            Positions.transformAt' pWGS84 (Epoch 2010.0) WGS84_G1762 tx15 `shouldBe` pWGS84
        it "uses the 15-parameter transformation and position epoch" $ do
            let pITRF2014 = Geocentric.metresPos 4027894.006 307045.600 4919474.910 ITRF2014
            let tx = Tx.params Txs.from_ITRF2014_to_ETRF2000
            let pETRF2000 = Positions.transformAt' pITRF2014 (Epoch 2012.0) ETRF2000 tx
            pETRF2000 `shouldBe`
                Geocentric.metresPos 4027894.366234 307045.252967 4919474.626307 ETRF2000
        it "returns the initial coordinates when doing round-trip (direct -> inverse)" $ do
            let tx = Tx.params Txs.from_ITRF2014_to_ETRF2000
            let itx = Tx.inverseParams tx
            let pITRF2014 = Geocentric.metresPos 4027894.006 307045.600 4919474.910 ITRF2014
            let e = Epoch 2019.0
            Positions.transformAt' (Positions.transformAt' pITRF2014 e ETRF2000 tx) e ITRF2014 itx `shouldBe`
                pITRF2014
        it "returns the initial coordinates when using chained conversion round-trip" $ do
            let pNAD83 =
                    Positions.toGeocentric (Geodetic.latLongHeightPos 0 0 Length.zero NAD83_CORS96)
            -- goes via ITRF2000
            let pITRF2014 =
                    fromJust
                        (Positions.transformAt pNAD83 (Epoch 2014.0) ITRF2014 Txs.timeDependent)
            Positions.transformAt pITRF2014 (Epoch 2014.0) NAD83_CORS96 Txs.timeDependent `shouldBe`
                Just pNAD83
        it "converts between ITRF2000 & ETRF2000" $ do
            let pITRF2000 = Geocentric.metresPos 4027894.006 307045.600 4919474.910 ITRF2000
            let pETRF2000 =
                    fromJust
                        (Positions.transformAt pITRF2000 (Epoch 2012) ETRF2000 Txs.timeDependent)
            pETRF2000 `shouldBe`
                Geocentric.metresPos 4027894.355909 307045.250849 4919474.644695 ETRF2000
        it "converts between ITRF2014 & ETRF2000" $ do
            let pITRF2014 = Geocentric.metresPos 4027894.006 307045.600 4919474.910 ITRF2014
            let pETRF2000 =
                    fromJust
                        (Positions.transformAt pITRF2014 (Epoch 2012) ETRF2000 Txs.timeDependent)
            pETRF2000 `shouldBe`
                Geocentric.metresPos 4027894.366234 307045.252967 4919474.626307 ETRF2000
