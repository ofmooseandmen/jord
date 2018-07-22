module Data.Geo.Jord.NedVectorSpec
    ( spec
    ) where

import Data.Geo.Jord
import Prelude hiding (length)
import Test.Hspec

spec :: Spec
spec = do
    describe "length" $
        it "computes the length of a NED vector" $
            length (nedVector (metres (-86126)) (metres (-78900)) (metres 1069)) `shouldBe` metres 116807.708
    describe "bearing" $
        it "computes the bearing of a NED vector" $
            bearing (nedVector (metres (-86126)) (metres (-78900)) (metres 1069)) `shouldBe` decimalDegrees 222.4927888
    describe "elevation" $
        it "computes the elevation of a NED vector from horizontal" $
            elevation (nedVector (metres (-86126)) (metres (-78900)) (metres 1069)) `shouldBe` decimalDegrees (-0.5243663)
