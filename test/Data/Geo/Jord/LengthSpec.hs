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
    describe "Reading invalid lengthss" $ do
        it "fails to read 5" $ readLengthM "5" `shouldBe` Nothing
    describe "Showing lengths" $ do
        it "shows length in metres when <= 10000 m" $ show (ofMetres 5) `shouldBe` "5.0m"
        it "shows length in kilometres when > 10000 m" $
            show (ofKilometres 1000) `shouldBe` "1000.0km"
    describe "Converting lengths" $ do
        it "converts metres to kilometres" $ kilometres (ofMetres 1000) `shouldBe` 1.0
