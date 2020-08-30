module Data.Geo.Jord.LengthSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Length as Length

spec :: Spec
spec = do
    describe "Reading valid lengths" $ do
        it "reads -15.2m" $ Length.read "-15.2m" `shouldBe` Just (Length.metres (-15.2))
        it "reads 154km" $ Length.read "154km" `shouldBe` Just (Length.kilometres 154)
        it "reads 1000nm" $ Length.read "1000nm" `shouldBe` Just (Length.nauticalMiles 1000)
        it "reads 25000ft" $ Length.read "25000ft" `shouldBe` Just (Length.feet 25000)
    describe "Reading invalid lengths" $ do
        it "fails to read 5" $ Length.read "5" `shouldBe` Nothing
        it "fails to read 5nmi" $ Length.read "5nmi" `shouldBe` Nothing
    describe "Showing lengths" $ do
        it "shows length in Length.metres when <= 10000 m" $
            show (Length.metres 5) `shouldBe` "5.0m"
        it "shows length in Length.kilometres when > 10000 m" $
            show (Length.kilometres 1000) `shouldBe` "1000.0km"
    describe "Converting lengths" $ do
        it "converts Length.metres to Length.kilometres" $
            Length.toKilometres (Length.metres 1000) `shouldBe` 1.0
        it "converts Length.metres to nautical miles" $
            Length.toNauticalMiles (Length.metres 1000) `shouldBe` 0.5399568034557235
        it "converts Length.kilometres to nautical miles" $
            Length.toNauticalMiles (Length.kilometres 1000) `shouldBe` 539.9568034557235
        it "converts nautical miles to Length.metres" $
            Length.toMetres (Length.nauticalMiles 10.5) `shouldBe` 19446
        it "converts nautical miles to Length.kilometres" $
            Length.toKilometres (Length.nauticalMiles 10.5) `shouldBe` 19.446
        it "converts Length.feet to Length.metres" $
            Length.toMetres (Length.feet 25000) `shouldBe` 7620
        it "converts Length.metres to Length.feet" $
            Length.toFeet (Length.metres 7620) `shouldBe` 25000
    describe "Resolution" $ do
        it "handles 1 kilometre" $ Length.toKilometres (Length.kilometres 1) `shouldBe` 1
        it "handles 1 metre" $ Length.toMetres (Length.metres 1) `shouldBe` 1
        it "handles 1 nautical mile" $ Length.toNauticalMiles (Length.nauticalMiles 1) `shouldBe` 1
        it "handles 1 foot" $ Length.toFeet (Length.feet 1) `shouldBe` 1
    describe "Adding/Subtracting lengths" $ do
        it "adds lengths" $
            Length.add (Length.kilometres 1000) (Length.metres 1000) `shouldBe`
            Length.metres 1001000
        it "subtracts lengths" $
            Length.subtract (Length.metres 1000) (Length.nauticalMiles 10.5) `shouldBe`
            Length.metres (-18446)
