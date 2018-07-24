module Data.Geo.Jord.GeodeticsSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Geodetic Problems - Ellipsoidal" $ do
        describe "delta (second or reverse problem)" $ do
            it "computes NED Vector between LatLong positions" $ do
                let p1 = latLongDecimal 49.66618 3.45063
                let p2 = latLongDecimal 48.88667 2.37472
                let d = delta p1 p2 wgs84
                d `shouldBe` nedVectorMetres (-86125.88) (-78900.087) 1069.198
            it "computes NED Vector between angular positions" $ do
                let p1 = latLongPos (latLongDecimal 49.66618 3.45063) 0
                let p2 = latLongPos (latLongDecimal 48.88667 2.37472) 0
                let d = delta p1 p2 wgs84
                d `shouldBe` nedVectorMetres (-86125.88) (-78900.087) 1069.198
    describe "Geodetic Problems - Spherical" $ do
        describe "destination (first or direct problem)" $ do
            it "return the given point if distance is 0 meter" $ do
                let p0 = readLatLong "531914N0014347W"
                let d = BearingDistance (decimalDegrees 96.0217, metres 0)
                destination p0 d (meanRadius wgs84) `shouldBe` p0
            it "return the angular position along great-circle at distance and bearing" $ do
                let p0 = latLongPos (readLatLong "531914N0014347W") 15000.0
                let d = BearingDistance (decimalDegrees 96.0217, metres 124800)
                let p1 = latLongPos (latLongDecimal 53.1882691 0.1332744) 15000.0
                destination p0 d (meanRadius wgs84) `shouldBe` p1
            it "return the ECEF position along great-circle at distance and bearing" $ do
                let p0 = ecefPosMetres 3825345.17 (-115519.77) 5103962.93
                let d = BearingDistance (decimalDegrees 96.0217, metres 124800)
                let p1 = ecefPosMetres 3833781.448 8385.502 5088398.79
                destination p0 d (meanRadius wgs84) `shouldBe` p1
        describe "delta (second or reverse problem)" $ do
            it "returns (0, 0) if both points are equal" $ do
                let p = readLatLong "500359N1795959W"
                delta p p (meanRadius wgs84) `shouldBe` BearingDistance (zero, zero)
            it "returns the (initial bearing, surface distance) between 2 LatLong" $ do
                let p1 = readLatLong "500359N0054253W"
                let p2 = readLatLong "583838N0030412W"
                delta p1 p2 (meanRadius wgs84) `shouldBe`
                    BearingDistance (decimalDegrees 9.1198181, metres 968854.868)
            it "returns the (initial bearing, surface distance) between 2 ECEF position" $ do
                let p1 = ecefPosMetres 4081816.66 (-408478.77) 4867532.37
                let p2 = ecefPosMetres 3322255.65 (-178182.35) 5423400.17
                delta p1 p2 (meanRadius wgs84) `shouldBe`
                    BearingDistance (decimalDegrees 9.14476499, metres 970986.478)
