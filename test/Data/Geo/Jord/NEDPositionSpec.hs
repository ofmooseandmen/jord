module Data.Geo.Jord.NEDPositionSpec
    ( spec
    ) where

import Data.Geo.Jord
import Prelude hiding (length)
import Test.Hspec

spec :: Spec
spec = do
    describe "length" $
        it "computes the length of a NED position" $
            length (nedPosition (metres (-86126)) (metres (-78900)) (metres 1069)) `shouldBe` metres 116807.708
    describe "bearing" $
        it "computes the bearing of a NED position" $
            bearing (nedPosition (metres (-86126)) (metres (-78900)) (metres 1069)) `shouldBe` decimalDegrees 222.4927888
    describe "elevation" $
        it "computes the elevation of a NED position from horizontal" $
            elevation (nedPosition (metres (-86126)) (metres (-78900)) (metres 1069)) `shouldBe` decimalDegrees (-0.5243663)
