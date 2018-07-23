module Data.Geo.Jord.GeodeticsSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec =
    describe "Geodetic Problems - Spherical" $ do
        describe "destination (first or direct problem)" $ do
            it "return the given point if distance is 0 meter" $ do
                let p0 = spherical84 (readLatLong "531914N0014347W")
                let d = BearingDistance (decimalDegrees 96.0217, metres 0)
                destination p0 d `shouldBe` p0
            it "return the angular position along great-circle at distance and bearing" $ do
                let p0 = spherical84 (angularPos (readLatLong "531914N0014347W") 15000.0)
                let d = BearingDistance (decimalDegrees 96.0217, metres 124800)
                let p1 = spherical84 (angularPos (latLongDecimal 53.1882691 0.1332744) 15000.0)
                destination p0 d `shouldBe` p1
            it "return the ECEF position along great-circle at distance and bearing" $ do
                let p0 = spherical84 (ecefPosMetres 3825345.17 (-115519.77) 5103962.93)
                let d = BearingDistance (decimalDegrees 96.0217, metres 124800)
                let p1 = spherical84 (ecefPosMetres 3833781.448 8385.502 5088398.79)
                destination p0 d `shouldBe` p1
        describe "delta (second or reverse problem)" $ do
            it "returns (0, 0) if both points are equal" $ do
                let p = spherical84 (readLatLong "500359N1795959W")
                delta p p `shouldBe` BearingDistance (zero, zero)
            it "returns the (initial bearing, surface distance) between 2 LatLong" $ do
                let p1 = spherical84 (readLatLong "500359N0054253W")
                let p2 = spherical84 (readLatLong "583838N0030412W")
                delta p1 p2 `shouldBe` BearingDistance (decimalDegrees 9.1198181, metres 968854.868)
            it "returns the (initial bearing, surface distance) between 2 ECEF position" $ do
                let p1 = spherical84 (ecefPosMetres 4081816.66 (-408478.77) 4867532.37)
                let p2 = spherical84 (ecefPosMetres 3322255.65 (-178182.35) 5423400.17)
                delta p1 p2 `shouldBe`
                    BearingDistance (decimalDegrees 9.14476499, metres 970986.478)
