module Data.Geo.Jord.GeocentricSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Ellipsoid as Ellipsoid
import Data.Geo.Jord.Ellipsoids
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Models

spec :: Spec
spec = do
    describe "antipode" $ do
        it "returns the antipodal position" $ do
            Geocentric.antipode
                (Geocentric.metresPos (-4069916.936) 1985031.122443 4497955.010584 WGS84) `shouldBe`
                Geocentric.metresPos 4069916.936 (-1985031.122443) (-4497955.010584) WGS84
        it "returns the south pole when called with the north pole" $ do
            Geocentric.antipode (Geocentric.northPole WGS84) `shouldBe` Geocentric.southPole WGS84
        it "returns the north pole when called with the south pole" $ do
            Geocentric.antipode (Geocentric.southPole WGS84) `shouldBe` Geocentric.northPole WGS84
    describe "poles" $ do
        it "returns (0, 0, polarRadius) for the north pole" $ do
            Geocentric.northPole WGS84 `shouldBe`
                Geocentric.metresPos 0 0 (Length.toMetres . Ellipsoid.polarRadius $ eWGS84) WGS84
        it "returns (0, 0, -polarRadius) for the south pole" $ do
            Geocentric.southPole WGS84 `shouldBe`
                Geocentric.metresPos
                    0
                    0
                    (negate . Length.toMetres . Ellipsoid.polarRadius $ eWGS84)
                    WGS84
