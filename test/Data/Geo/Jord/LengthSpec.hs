module Data.Geo.Jord.LengthSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid lengths" $ do
        it "reads -15.2m" $ readLength "-15.2m" `shouldBe` metres (-15.2)
        it "reads 154km" $ readLength "154km" `shouldBe` kilometres 154
        it "reads 1000Nm" $ readLength "1000Nm" `shouldBe` nauticalMiles 1000
        it "reads 25000ft" $ readLength "25000ft" `shouldBe` feet 25000
    describe "Reading invalid lengths" $ do
        it "fails to read 5" $ readLengthE "5" `shouldBe` Left "couldn't read length 5"
        it "fails to read 5nmi" $ readLengthE "5nmi" `shouldBe` Left "couldn't read length 5nmi"
    describe "Showing lengths" $ do
        it "shows length in metres when <= 10000 m" $ show (metres 5) `shouldBe` "5.0m"
        it "shows length in kilometres when > 10000 m" $
            show (kilometres 1000) `shouldBe` "1000.0km"
    describe "Converting lengths" $ do
        it "converts metres to kilometres" $ toKilometres (metres 1000) `shouldBe` 1.0
        it "converts metres to nautical miles" $
            toNauticalMiles (metres 1000) `shouldBe` 0.5399568034557235
        it "converts kilometres to nautical miles" $
            toNauticalMiles (kilometres 1000) `shouldBe` 539.9568034557235
        it "converts nautical miles to metres" $ toMetres (nauticalMiles 10.5) `shouldBe` 19446
        it "converts nautical miles to kilometres" $
            toKilometres (nauticalMiles 10.5) `shouldBe` 19.446
        it "converts feet to metres" $
            toMetres (feet 25000) `shouldBe` 7620
        it "converts metres to feet" $
            toFeet (metres 7620) `shouldBe` 25000
    describe "Adding/Subtracting lengths" $ do
        it "adds lengths" $ add (kilometres 1000) (metres 1000) `shouldBe` metres 1001000
        it "subtracts lengths" $ sub (metres 1000) (nauticalMiles 10.5) `shouldBe` metres (-18446)
