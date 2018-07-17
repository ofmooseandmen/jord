module Data.Geo.Jord.EcefSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Geodetic To ECEF" $
        it "transforms geodetic position and height to ECEF vector" $
        geodeticToEcef refGeodetic wgs84 `shouldBe` refEcef
    describe "ECEF To Geodetic" $
        it "transforms ECEF vector to geodetic position and height" $
        ecefToGeodetic refEcef wgs84 `shouldBe` refGeodetic

refGeodetic :: (LatLong, Double)
refGeodetic = (latLongDecimal 39.379 (-48.013), 4702059.834)

refEcef :: Vector3d
refEcef = Vector3d (0.9 * 6371e3)  ((-1.0) * 6371e3) (1.1 * 6371e3)
