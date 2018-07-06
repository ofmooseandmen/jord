module Data.Geo.Jord.EvalSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec = do
    describe "Expression evaluation" $ do
        it "evaluates simple expression" $
            case eval "antipode 54N154E" of
                (Right (Geo g)) -> g `geoShouldBe` geoPos (-54.0) (-26.0)
                r -> fail (show r)
        it "evaluates expression with nested function calls" $
            case eval
                     "finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E)" of
                (Right (Ang a)) -> a `angleShouldBe` ofDegrees 126 -- TODO this should be 125.6839436
                r -> fail (show r)
        it "rejects expression with lexical error" $
            case eval "finalBearing (destination [" of
                (Left e) -> e `shouldBe` "Lexical error: [)"
                r -> fail (show r)
        it "rejects expression with syntaxic error" $
            case eval "finalBearing (destination a" of
                (Left e) -> e `shouldBe` "Syntax error: ')' not found"
                r -> fail (show r)
