module Data.Geo.Jord.NedVectorSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "norm" $
        it "computes the norm of a NED vector" $
        norm (nedVectorMetres (-86126) (-78900) 1069) `shouldBe` metres 116807.708
    describe "bearing" $
        it "computes the bearing of a NED vector" $
        bearing (nedVectorMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees 222.4927888
    describe "elevation" $
        it "computes the elevation of a NED vector from horizontal" $
        elevation (nedVectorMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees (-0.5243663)
