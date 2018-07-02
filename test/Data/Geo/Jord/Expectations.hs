module Data.Geo.Jord.Expectations
    ( angleShouldBe
    , geoShouldBe
    , lengthShouldBe
    ) where

import Control.Monad
import Data.Geo.Jord
import Test.HUnit
import Test.Hspec

angleShouldBe :: (HasCallStack) => Angle -> Angle -> Expectation
actual `angleShouldBe` expected = assertDoubleEquals tolerance (degrees expected) (degrees actual)

geoShouldBe :: (HasCallStack) => GeoPos -> GeoPos -> Expectation
actual `geoShouldBe` expected = do
    latitude actual `angleShouldBe` latitude expected
    longitude actual `angleShouldBe` longitude expected

lengthShouldBe :: (HasCallStack) => Length -> Length -> Expectation
actual `lengthShouldBe` expected = assertDoubleEquals tolerance (metres expected) (metres actual)

assertDoubleEquals ::
       Double -- ^ The maximum difference between expected and actual
    -> Double -- ^ The expected value
    -> Double -- ^ The actual value
    -> Expectation
assertDoubleEquals delta expected actual =
    unless (abs (expected - actual) < delta) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ " +/- " ++ show delta ++ "\n but got: " ++ show actual

-- | tolerance to assert meters and degrees. At the equator 1 degree of latitude
-- is 111 000 meters.
tolerance :: Double
tolerance = 1e-7
