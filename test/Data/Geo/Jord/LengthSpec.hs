module Data.Geo.Jord.LengthSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid lengths" $ do
        it "reads -15.2m" $ readLength "-15.2m" `lengthShouldBe` ofMetres (-15.2)
        it "reads 154km" $ readLength "154km" `lengthShouldBe` ofKilometres 154
        it "reads 1000Nm" $ readLength "1000Nm" `lengthShouldBe` ofNauticalMiles 1000
    describe "Reading invalid lengths" $ do
        it "fails to read 5" $ readLengthM "5" `shouldBe` Nothing
        it "fails to read 5nmi" $ readLengthM "5nmi" `shouldBe` Nothing
    describe "Showing lengths" $ do
        it "shows length in metres when <= 10000 m" $ show (ofMetres 5) `shouldBe` "5.0m"
        it "shows length in kilometres when > 10000 m" $
            show (ofKilometres 1000) `shouldBe` "1000.0km"
    describe "Converting lengths" $ do
        it "converts metres to kilometres" $ kilometres (ofMetres 1000) `shouldBe` 1.0
        it "converts metres to nautical miles" $ nauticalMiles (ofMetres 1000) `shouldBe` 0.5399568034557235
        it "converts kilometres to nautical miles" $ nauticalMiles (ofKilometres 1000) `shouldBe` 539.9568034557235
        it "converts nautical miles to metres" $ metres (ofNauticalMiles 10.5) `shouldBe` 19446
        it "converts nautical miles to kilometres" $ kilometres (ofNauticalMiles 10.5) `shouldBe` 19.446
    describe "Adding/Subtracting lengths" $ do
        it "adds lengths" $ add (ofKilometres 1000) (ofMetres 1000) `lengthShouldBe` ofMetres 1001000
        it "subtracts lengths" $ sub (ofMetres 1000) (ofNauticalMiles 10.5) `lengthShouldBe` ofMetres (-18446)
