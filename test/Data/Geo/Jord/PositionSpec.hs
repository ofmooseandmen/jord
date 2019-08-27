module Data.Geo.Jord.PositionSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "Geodetic <=> Geocentric (Ellipsoidal)" $ do
        it "n-vector <=> ECEF" $ do
            let p = nvectorPos 0.5 0.5 0.7071 WGS84
            let e = ecefMetresPos 3194434.411 3194434.411 4487326.8195 WGS84
            p `shouldBe` e
        it "latitude, longitude and height <=> ECEF" $ do
            let refLlh =
                    [ decimalLatLongHeightPos 0 0 zero WGS84
                    , decimalLatLongHeightPos 90 0 zero WGS84
                    , decimalLatLongHeightPos (-90) 0 zero WGS84
                    , decimalLatLongHeightPos 45.0 45.0 (metres 500) WGS84
                    , decimalLatLongHeightPos (-45) (-45) (metres 500) WGS84
                    ]
            let refEcefs =
                    [ ecefMetresPos 6378137 0 0 WGS84
                    , ecefMetresPos 0 0 6356752.3142 WGS84
                    , ecefMetresPos 0 0 (-6356752.3142) WGS84
                    , ecefMetresPos 3194669.1450999997 3194669.1450999997 4487701.9622 WGS84
                    , ecefMetresPos 3194669.1450999997 (-3194669.1450999997) (-4487701.9622) WGS84
                    ]
            refLlh `shouldBe` refEcefs
    describe "Geodetic <=> Geocentric (Spherical)" $ do
        it "n-vector <=> ECEF" $ do
            let p = nvectorPos 0.5 0.5 0.7071 S84
            let e = ecefMetresPos 3185519.6603 3185519.6603 4504961.9036 S84
            p `shouldBe` e
        it "latitude, longitude and height <=> ECEF" $ do
            let refLlh =
                    [ decimalLatLongHeightPos 0 0 zero S84
                    , decimalLatLongHeightPos 90 0 zero S84
                    , decimalLatLongHeightPos (-90) 0 zero S84
                    , decimalLatLongHeightPos 45.0 45.0 (metres 500) S84
                    , decimalLatLongHeightPos (-45) (-45) (metres 500) S84                    ]
            let refEcefs =
                    [ ecefMetresPos 6371008.7714 0 0 S84
                    , ecefMetresPos 0 0 6371008.7714 S84
                    , ecefMetresPos 0 0 (-6371008.7714) S84
                    , ecefMetresPos 3185754.3857 3185754.3857 4505337.0586 S84
                    , ecefMetresPos 3185754.3857 (-3185754.3857) (-4505337.0586) S84
                    ]
            refLlh `shouldBe` refEcefs