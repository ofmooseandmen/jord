module Data.Geo.Jord.PositionSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Models (Epoch(..), S84(..), WGS84(..))
import qualified Data.Geo.Jord.Position as Position

spec :: Spec
spec = do
    describe "Geodetic <=> Geocentric (Ellipsoidal)" $ do
        it "n-vector <=> Geocentric" $ do
            let p = Geodetic.nvectorPos 0.5 0.5 0.7071067811865475 WGS84
            let g = Geocentric.metresPos 3194419.145061 3194419.145061 4487348.408866 WGS84
            Position.toGeocentric p `shouldBe` g
            Position.toGeodetic g `shouldBe` p
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
            fmap toGeocentric refLlh `shouldBe` refGeocentrics
            fmap toGeodetic refGeocentrics `shouldBe` refLlh
    describe "Geodetic <=> Geocentric (Spherical)" $ do
        it "n-vector <=> Geocentric" $ do
            let p = Geodetic.nvectorPos 0.5 0.5 0.7071 S84
            let g = Geocentric.metresPos 3185519.660307 3185519.660307 4504961.903617 S84
            Position.toGeocentric p `shouldBe` g
            Position.toGeodetic g `shouldBe` p
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
            fmap Position.toGeocentric refLlh `shouldBe` refGeocentrics
            fmap Position.toGeodetic refGeocentrics `shouldBe` refLlh
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
            let pWGS84 =
                    Geocentric.metresPos 4193790.895437 454436.195118 4768166.813801 WGS84_G1762
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
