module Data.Geo.Jord.PositionSpec
    ( spec
    ) where

import Control.Monad
import Data.Geo.Jord
import Test.HUnit
import Test.Hspec

spec :: Spec
spec = do
    describe "Geodetic position to ECEF position" $
        it "transforms geodetic position and height to ECEF position" $ do
            let refGeodetics =
                    [ geodeticPosition (latLongDecimal 39.379 (-48.013)) 4702059.834 wgs84
                    , geodeticPosition (latLongDecimal 45.0 45.0) 0 wgs84
                    , geodeticPosition (latLongDecimal 48.8562 2.3508) 67.36972232195099 wgs84
                    ]
            let refEcefs =
                    [ ecefPosition 5733855.77488171 (-6370998.38026088) 7008137.51062469 wgs84
                    , ecefPosition 3194419.14512197 3194419.14512197 4487348.40860601 wgs84
                    , ecefPosition 4200996.76974058 172460.32072401 4780102.80780980 wgs84
                    ]
            mapM_ (\(g, e) -> toEcef g `ecefShouldBe` e) (zip refGeodetics refEcefs)
    describe "ECEF position To geodetic position" $
        it "transforms ECEF position to geodetic position and height" $ do
            let refGeodetics =
                    [ geodeticPosition (latLongDecimal 39.379 (-48.013)) 4702059.833901506 wgs84
                    , geodeticPosition (latLongDecimal 45.0 45.0) (-1.2238394158266557e-4) wgs84
                    , geodeticPosition (latLongDecimal 48.8562 2.3508) 67.36972232195099 wgs84
                    ]
            let refEcefs =
                    [ ecefPosition 5733855.774881717 (-6370998.380260889) 7008137.510624695 wgs84
                    , ecefPosition 3194419.145121972 3194419.145121971 4487348.408606014 wgs84
                    , ecefPosition 4200996.769831858 172460.320727757 4780102.807914356 wgs84
                    ]
            mapM_ (\(g, e) -> fromEcef e `shouldBe` g) (zip refGeodetics refEcefs)

ecefShouldBe :: (HasCallStack) => EcefPosition -> EcefPosition -> Expectation
actual `ecefShouldBe` expected = do
    assertDoubleEqual tolerance (ex expected) (ex actual)
    assertDoubleEqual tolerance (ey expected) (ey actual)
    assertDoubleEqual tolerance (ez expected) (ez actual)
    (ellipsoid expected) @?= (ellipsoid actual)

assertDoubleEqual ::
       Double -- ^ The maximum difference between expected and actual
    -> Double -- ^ The expected value
    -> Double -- ^ The actual value
    -> Expectation
assertDoubleEqual delta expected actual =
    unless (abs (expected - actual) < delta) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ " +/- " ++ show delta ++ "\n but got: " ++ show actual

tolerance :: Double
tolerance = 0.00000001
