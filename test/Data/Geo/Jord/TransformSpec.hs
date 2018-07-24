module Data.Geo.Jord.TransformSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Ellipsoidal transformation between coordinates systems" $ do
        it "transforms NVector position to ECEF position" $ do
            let p = NVector 0.5 0.5 0.7071
            toEcef p wgs84 `shouldBe`
                ecefPosMetres 3194434.411029306 3194434.411029306 4487326.819249299
        it "transforms angular position to ECEF position" $ do
            let refAngular =
                    [ latLongPos (latLongDecimal 39.379 (-48.013)) 4702059.834
                    , latLongPos (latLongDecimal 45.0 45.0) 0
                    , latLongPos (latLongDecimal 48.8562 2.3508) 67.36972232195099
                    ]
            let refEcefs =
                    [ ecefPosMetres 5733855.77488171 (-6370998.38026088) 7008137.51062469
                    , ecefPosMetres 3194419.14512197 3194419.14512197 4487348.40860601
                    , ecefPosMetres 4200996.76974058 172460.32072401 4780102.80780980
                    ]
            mapM_ (\(a, e) -> toEcef a wgs84 `shouldBe` e) (zip refAngular refEcefs)
        it "transforms ECEF position to angular position" $ do
            let refAngular =
                    [ latLongPos (latLongDecimal 39.379 (-48.013)) 4702059.834050887
                    , latLongPos (latLongDecimal 45.0 45.0) 3.423524451828947e-5
                    , latLongPos (latLongDecimal 48.8562 2.3508) 67.36990469945641
                    ]
            let refEcefs =
                    [ ecefPosMetres 5733855.774881717 (-6370998.380260889) 7008137.510624695
                    , ecefPosMetres 3194419.145121972 3194419.145121971 4487348.408606014
                    , ecefPosMetres 4200996.769831858 172460.320727757 4780102.807914356
                    ]
            mapM_ (\(a, e) -> fromEcef e wgs84 `shouldBe` a) (zip refAngular refEcefs)
    describe "Spherical transformation between coordinates systems" $ do
        it "transforms NVector position to ECEF position" $ do
            let p = NVector 0.5 0.5 0.7071
            toEcef p (meanRadius wgs84) `shouldBe` ecefPosMetres 3185519.66 3185519.66 4504961.903
        it "transforms angular position to ECEF position" $ do
            let refAngular =
                    [ latLongPos (latLongDecimal 39.379 (-48.013)) 4702059.834
                    , latLongPos (latLongDecimal 45.0 45.0) 0
                    , latLongPos (latLongDecimal 48.8562 2.3508) 67.36972232195099
                    ]
            let refEcefs =
                    [ ecefPosMetres 5725717.354 (-6361955.623) 7025277.914
                    , ecefPosMetres 3185504.386 3185504.386 4504983.505
                    , ecefPosMetres 4188328.891 171940.276 4797806.669
                    ]
            mapM_ (\(a, e) -> toEcef a (meanRadius wgs84) `shouldBe` e) (zip refAngular refEcefs)
        it "transforms ECEF position to angular position" $ do
            let refAngular =
                    [ latLongPos (latLongDecimal 39.379 (-48.013)) 4702059.834000001
                    , latLongPos (latLongDecimal 45.0 45.0) 1.0000001639127731e-3
                    , latLongPos (latLongDecimal 48.8562 2.3508) 67.37000000011176
                    ]
            let refEcefs =
                    [ ecefPosMetres 5725717.354 (-6361955.623) 7025277.914
                    , ecefPosMetres 3185504.386 3185504.386 4504983.505
                    , ecefPosMetres 4188328.891 171940.276 4797806.669
                    ]
            mapM_ (\(a, e) -> fromEcef e (meanRadius wgs84) `shouldBe` a) (zip refAngular refEcefs)
