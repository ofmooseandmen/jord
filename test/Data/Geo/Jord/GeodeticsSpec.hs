module Data.Geo.Jord.GeodeticsSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "target" $ do
        it "return the given point if NED vector vnorm = 0" $ do
            let p0 = readLatLong "531914N0014347W"
            let d = ned zero zero zero
            target p0 d wgs84 `shouldBe` p0
        it "computes the target point from p0 and NED vector" $ do
            let p0 = decimalLatLong 49.66618 3.45063
            let d = nedMetres (-86126) (-78900) 1069
            target p0 d wgs84 `shouldBe` decimalLatLong 48.8866688 2.374721111
    describe "delta" $ do
        it "computes NED Vector between LatLong positions" $ do
            let p1 = decimalLatLong 49.66618 3.45063
            let p2 = decimalLatLong 48.88667 2.37472
            let d = delta p1 p2 wgs84
            d `shouldBe` nedMetres (-86125.88049540376) (-78900.08718759022) 1069.1981930266265
        it "computes NED Vector between angular positions" $ do
            let p1 = decimalLatLongHeight 49.66618 3.45063 zero
            let p2 = decimalLatLongHeight 48.88667 2.37472 zero
            let d = delta p1 p2 wgs84
            d `shouldBe` nedMetres (-86125.88049540376) (-78900.08718759022) 1069.1981930266265
    describe "delta and target consistency" $
        it "computes target p1 (delta p1 p2) = p2" $ do
            let p1 = decimalLatLongHeight 49.66618 3.45063 zero
            let p2 = decimalLatLongHeight 48.88667 2.37472 zero
            target p1 (delta p1 p2 wgs84) wgs84 `shouldBe` p2
