module Data.Geo.Jord.PositionSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "antipode" $ do
        it "returns the antipodal position" $ do
            antipode (wgs84Pos 45 154 (metres 15000)) `shouldBe` wgs84Pos (-45) (-26) (metres 15000)
            antipode (s84Pos 45 154 (metres 15000)) `shouldBe` s84Pos (-45) (-26) (metres 15000)
        it "returns the south pole when called with the north pole" $ do
            antipode (northPole WGS84) `shouldBe` southPole WGS84
            antipode (northPole S84) `shouldBe` southPole S84
        it "returns the north pole when called with the south pole" $ do
            antipode (southPole WGS84) `shouldBe` northPole WGS84
            antipode (southPole S84) `shouldBe` northPole S84
    describe "wrapping latitude/longitude" $ do
        it "wraps a Earth position to [-90°, 90°] and [-180°, 180°]" $
            latLong (s84Pos 91 54 zero) `shouldBe` (89, -126)
        it "wraps a Mars position to [-90°, 90°] and [0°, 360°]" $
            latLong (latLongPos 91 (-150) Mars2000) `shouldBe` (89, 210)
    describe "Geodetic <=> Geocentric (Ellipsoidal)" $ do
        it "n-vector <=> Geocentric" $ do
            let p = nvectorPos 0.5 0.5 0.7071 WGS84
            let g = geocentricMetresPos 3194434.411 3194434.411 4487326.8195 WGS84
            p `shouldBe` g
        it "latitude, longitude and height <=> Geocentric" $ do
            let refLlh =
                    [ latLongHeightPos 0 0 zero WGS84
                    , latLongHeightPos 90 0 zero WGS84
                    , latLongHeightPos (-90) 0 zero WGS84
                    , latLongHeightPos 45.0 45.0 (metres 500) WGS84
                    , latLongHeightPos (-45) (-45) (metres 500) WGS84
                    ]
            let refGeocentrics =
                    [ geocentricMetresPos 6378137 0 0 WGS84
                    , geocentricMetresPos 0 0 6356752.3142 WGS84
                    , geocentricMetresPos 0 0 (-6356752.3142) WGS84
                    , geocentricMetresPos 3194669.1450999997 3194669.1450999997 4487701.9622 WGS84
                    , geocentricMetresPos
                          3194669.1450999997
                          (-3194669.1450999997)
                          (-4487701.9622)
                          WGS84
                    ]
            refLlh `shouldBe` refGeocentrics
    describe "Geodetic <=> Geocentric (Spherical)" $ do
        it "n-vector <=> Geocentric" $ do
            let p = nvectorPos 0.5 0.5 0.7071 S84
            let g = geocentricMetresPos 3185519.6603 3185519.6603 4504961.9036 S84
            p `shouldBe` g
        it "latitude, longitude and height <=> Geocentric" $ do
            let refLlh =
                    [ latLongHeightPos 0 0 zero S84
                    , latLongHeightPos 90 0 zero S84
                    , latLongHeightPos (-90) 0 zero S84
                    , latLongHeightPos 45.0 45.0 (metres 500) S84
                    , latLongHeightPos (-45) (-45) (metres 500) S84
                    ]
            let refGeocentrics =
                    [ geocentricMetresPos 6371008.7714 0 0 S84
                    , geocentricMetresPos 0 0 6371008.7714 S84
                    , geocentricMetresPos 0 0 (-6371008.7714) S84
                    , geocentricMetresPos 3185754.3857 3185754.3857 4505337.0586 S84
                    , geocentricMetresPos 3185754.3857 (-3185754.3857) (-4505337.0586) S84
                    ]
            refLlh `shouldBe` refGeocentrics