module Data.Geo.Jord.GeodesicEllipsoidalSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "antipode" $ do
        it "returns the antipodal position" $ do
            let p = decimalLatLongHeightPos 45 154 (metres 15000) WGS84
            let e = decimalLatLongHeightPos (-45) (-26) (metres 15000) WGS84
            antipode p `shouldBe` e
        it "returns the south pole when called with the north pole" $
            antipode (northPole WGS84) `shouldBe` southPole WGS84
        it "returns the north pole when called with the south pole" $
            antipode (southPole WGS84) `shouldBe` northPole WGS84