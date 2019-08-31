module Data.Geo.Jord.GeodesicEllipsoidalSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec =
    describe "antipode" $ do
        it "returns the antipodal position" $ do
            let p = wgs84Pos 45 154 (metres 15000)
            let e = wgs84Pos (-45) (-26) (metres 15000)
            antipode p `shouldBe` e
        it "returns the south pole when called with the north pole" $
            antipode (northPole WGS84) `shouldBe` southPole WGS84
        it "returns the north pole when called with the south pole" $
            antipode (southPole WGS84) `shouldBe` northPole WGS84