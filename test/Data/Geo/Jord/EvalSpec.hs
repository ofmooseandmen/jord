module Data.Geo.Jord.EvalSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Expression evaluation" $ do
        it "evaluates simple expression" $
            case eval "antipode 54N154E" emptyVault of
                (Right (Llh llh)) -> llh `shouldBe` decimalLatLongHeight (-54.0) (-26.0) zero
                r -> fail (show r)
        it "evaluates an expression with one function call" $
            case eval "surfaceDistance 54N154E (antipode 54N154E)" emptyVault of
                (Right (Len l)) -> l `shouldBe` kilometres 20015.114351
                r -> fail (show r)
        it "evaluates an expression with one function call" $
            case eval "surfaceDistance (antipode 54N154E) 54N154E" emptyVault of
                (Right (Len l)) -> l `shouldBe` kilometres 20015.114351
                r -> fail (show r)
        it "evaluates expression with nested function calls" $
            case eval
                     "finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readLatLong 54°N,154°E)"
                     emptyVault of
                (Right (Ang a)) -> a `shouldBe` decimalDegrees 126
                r -> fail (show r)
        it "resolves variables" $ do
            let vault = insert "a" (Llh (decimalLatLongHeight 54.0 154.0 zero)) emptyVault
            case eval "antipode a" vault of
                (Right (Llh llh)) -> llh `shouldBe` decimalLatLongHeight (-54.0) (-26.0) zero
                r -> fail (show r)
        it "rejects expression with lexical error" $
            case eval "finalBearing(destination" emptyVault of
                (Left e) -> e `shouldBe` "Lexical error: finalBearing(destination"
                r -> fail (show r)
        it "rejects expression with syntaxic error" $
            case eval "finalBearing (destination a" emptyVault of
                (Left e) -> e `shouldBe` "Syntax error: ')' not found"
                r -> fail (show r)
