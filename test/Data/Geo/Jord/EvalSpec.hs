module Data.Geo.Jord.EvalSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Expression evaluation" $ do
        it "evaluates simple expression" $
            case eval' "antipode 54N154E" of
                (Right (Geo g)) -> g `shouldBe` gp (-54.0) (-26.0)
                r -> fail (show r)
        it "evaluates expression with nested function calls" $
            case eval'
                     "finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E)" of
                (Right (Ang a)) -> a `shouldBe` decimalDegrees 126
                r -> fail (show r)
        it "resolves variables" $
            case eval "antipode a" (\_ -> Just (Geo (gp 54.0 154.0))) of
                (Right (Geo g)) -> g `shouldBe` gp (-54.0) (-26.0)
                r -> fail (show r)
        it "rejects expression with lexical error" $
            case eval' "finalBearing(destination" of
                (Left e) -> e `shouldBe` "Lexical error: finalBearing(destination)"
                r -> fail (show r)
        it "rejects expression with syntaxic error" $
            case eval' "finalBearing (destination a" of
                (Left e) -> e `shouldBe` "Syntax error: ')' not found"
                r -> fail (show r)

gp :: Double -> Double -> GeoPos
gp lat lon = geoPos (decimalDegrees lat) (decimalDegrees lon)
