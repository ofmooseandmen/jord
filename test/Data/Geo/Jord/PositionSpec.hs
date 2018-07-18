module Data.Geo.Jord.PositionSpec
    ( spec
    ) where

import Control.Monad
import Data.Geo.Jord
import Test.HUnit
import Test.Hspec

spec :: Spec
spec = do
    describe "Geodetic To ECEF" $
        it "transforms geodetic position and height to ECEF vector" $ do
            let refGeodetics =
                    [ latLongHeight (latLongDecimal 39.379 (-48.013)) 4702059.834
                    , latLongHeight (latLongDecimal 45.0 45.0) 0
                    , latLongHeight (latLongDecimal 48.8562 2.3508) 67.36972232195099
                    ]
            let refEcefs =
                    [ ecefVector 5733855.77488171 (-6370998.38026088) 7008137.51062469
                    , ecefVector 3194419.14512197 3194419.14512197 4487348.40860601
                    , ecefVector 4200996.76974058 172460.32072401 4780102.80780980
                    ]
            mapM_ (\(g, e) -> geodeticToEcef g wgs84 `ecefShouldBe` e) (zip refGeodetics refEcefs)
    describe "ECEF To Geodetic" $
        it "transforms ECEF vector to geodetic position and height" $ do
            let refGeodetics =
                    [ latLongHeight (latLongDecimal 39.379 (-48.013)) 4702059.833901506
                    , latLongHeight (latLongDecimal 45.0 45.0) (-1.2238394158266557e-4)
                    , latLongHeight (latLongDecimal 48.8562 2.3508) 67.36972232195099
                    ]
            let refEcefs =
                    [ ecefVector 5733855.774881717 (-6370998.380260889) 7008137.510624695
                    , ecefVector 3194419.145121972 3194419.145121971 4487348.408606014
                    , ecefVector 4200996.769831858 172460.320727757 4780102.807914356
                    ]
            mapM_ (\(g, e) -> ecefToGeodetic e wgs84 `shouldBe` g) (zip refGeodetics refEcefs)

ecefShouldBe :: (HasCallStack) => EcefVector -> EcefVector -> Expectation
actual `ecefShouldBe` expected = do
    assertDoubleEquals tolerance (ex expected) (ex actual)
    assertDoubleEquals tolerance (ey expected) (ey actual)
    assertDoubleEquals tolerance (ez expected) (ez actual)

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
tolerance = 0.00000001
