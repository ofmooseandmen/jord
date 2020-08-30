module Data.Geo.Jord.ConversionSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord.Conversion
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Models (S84(..), WGS84(..))

spec :: Spec
spec = do
    describe "Geodetic <=> Geocentric (Ellipsoidal)" $ do
        it "n-vector <=> Geocentric" $ do
            let p = Geodetic.nvectorPos 0.5 0.5 0.7071067811865475 WGS84
            let g = Geocentric.metresPos 3194419.145061 3194419.145061 4487348.408866 WGS84
            toGeocentric p `shouldBe` g
            toGeodetic g `shouldBe` p
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
            toGeocentric p `shouldBe` g
            toGeodetic g `shouldBe` p
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
            fmap toGeocentric refLlh `shouldBe` refGeocentrics
            fmap toGeodetic refGeocentrics `shouldBe` refLlh
