module Data.Geo.Jord.Expectations
    ( geoShouldBe
    , metersShouldBe
    , degreesShouldBe
    ) where

import Control.Monad
import Data.Geo.Jord
import Test.HUnit
import Test.Hspec

geoShouldBe :: (HasCallStack) => GeoPos -> GeoPos -> Expectation
actual `geoShouldBe` expected = do
    latitude actual `degreesShouldBe` latitude expected
    longitude actual `degreesShouldBe` longitude expected

metersShouldBe :: (HasCallStack) => Meters -> Meters -> Expectation
actual `metersShouldBe` expected = assertDoubleEquals tolerance (meters expected) (meters actual)

degreesShouldBe :: (HasCallStack) => Degrees -> Degrees -> Expectation
actual `degreesShouldBe` expected = assertDoubleEquals tolerance (degrees expected) (degrees actual)

assertDoubleEquals ::
       Double -- ^ The maximum difference between expected and actual
    -> Double -- ^ The expected value
    -> Double -- ^ The actual value
    -> Expectation
assertDoubleEquals delta expected actual =
    unless (abs (expected - actual) < delta) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ " +/- " ++ show delta ++ "\n but got: " ++ show actual

tolerance :: Double
tolerance = 0.00001
