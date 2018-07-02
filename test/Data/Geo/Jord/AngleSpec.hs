module Data.Geo.Jord.AngleSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Geo.Jord.Expectations
import Test.Hspec

spec :: Spec
spec = do
    describe "Reading valid angles" $ do
        it "reads 55°36'21''" $ readAngle "55°36'21''" `angleShouldBe` ofDegrees 55.6058333
        it "reads 55.6058333°" $ readAngle "55.6058333°" `angleShouldBe` ofDegrees 55.6058333
