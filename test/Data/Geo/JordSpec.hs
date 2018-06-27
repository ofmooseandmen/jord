module Data.Geo.JordSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

-- | Tests geodesy calculation.
spec :: Spec
spec =
    describe "Jord" $ do
        describe "antipode" $ do
            it "returns the antipodal point" $
                antipode (read "484137N0061105E" :: GeoPos)
                `shouldBe`
                geo (-48.69361111111112) (-173.8152777777778)
            it "returns the south pole when called with the north pole" $
                antipode (north :: GeoPos) `shouldBe` geo (-90.0) (-180.0)
            it "returns the north pole when called with the south pole" $
                antipode (south :: GeoPos) `shouldBe` geo 90.0 (-180.0)
        describe "distance" $ do
            it "returns the distance between 2 points" $
                distance (read "500359N0054253W" :: GeoPos) (read "583838N0030412W" :: GeoPos)
                `shouldBe`
                Meters 968854.8849506531
            it "handles singularity at the pole" $
                distance (north :: GeoPos) (south :: GeoPos)
                `shouldBe`
                Meters 2.0015114442035925e7
            it "handles the discontinuity at the Date Line" $
                distance (read "500359N1795959W" :: GeoPos) (read "500359N1795959E" :: GeoPos)
                `shouldBe`
                Meters 39.65337387228698
        describe "north pole" $ do
            it "returns the (90, 0)" $
                (north :: GeoPos) `shouldBe` geo 90.0 0.0
        describe "south pole" $ do
            it "returns the (-90, 0)" $
                (south :: GeoPos) `shouldBe` geo (-90.0) 0.0
