module Data.Geo.Jord.Expr.TokenSpec
    ( spec
    ) where

import Data.Geo.Jord.Expr.Token

import Test.Hspec

spec :: Spec
spec = do
    describe "Tokenise" $ do
        it "foo" $
            tokenise
                "destination (readGeoPos P1) (finalBearing P2 P3) (distance (antipode P4) (readGeoPos P5))" `shouldBe`
            Right
                [ Func "destination"
                , Paren '('
                , Func "readGeoPos"
                , Str "P1"
                , Paren ')'
                , Paren '('
                , Func "finalBearing"
                , Str "P2"
                , Str "P3"
                , Paren ')'
                , Paren '('
                , Func "distance"
                , Paren '('
                , Func "antipode"
                , Str "P4"
                , Paren ')'
                , Paren '('
                , Func "readGeoPos"
                , Str "P5"
                , Paren ')'
                , Paren ')'
                ]
        it "fails if text contains unknown token" $
            tokenise "initialBearing [1" `shouldBe` Left "Invalid text: [1"
