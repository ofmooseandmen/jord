module Data.Geo.Jord.TransformationSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Ellipsoidal transformation between coordinates systems" $ do
        it "transforms NVector position to ECEF position" $ do
            let p = nvector 0.5 0.5 0.7071
            toEcef p wgs84 `shouldBe` ecefMetres 3194434.411 3194434.411 4487326.8195
        it "transforms angular position to ECEF position" $ do
            let refAngular =
                    [ decimalLatLongHeight 39.379 (-48.013) (metres 4702059.834)
                    , decimalLatLongHeight 45.0 45.0 zero
                    , decimalLatLongHeight 48.8562 2.3508 (metres 67.36972232195099)
                    ]
            let refEcefs =
                    [ ecefMetres 5733855.7748 (-6370998.3802) 7008137.5108
                    , ecefMetres 3194419.1451 3194419.1451 4487348.4088
                    , ecefMetres 4200996.7697 172460.3207 4780102.808
                    ]
            mapM_ (\(a, e) -> toEcef a wgs84 `shouldBe` e) (zip refAngular refEcefs)
        it "transforms ECEF position to angular position" $ do
            let refAngular =
                    [ decimalLatLongHeight 39.379 (-48.013) (metres 4702059.8339)
                    , decimalLatLongHeight 45.0 45.0 (metres (-0.0001))
                    , decimalLatLongHeight 48.8562 2.3508 (metres 67.3697)
                    ]
            let refEcefs =
                    [ ecefMetres 5733855.774881717 (-6370998.380260889) 7008137.510624695
                    , ecefMetres 3194419.145121972 3194419.145121971 4487348.408606014
                    , ecefMetres 4200996.769831858 172460.320727757 4780102.807914356
                    ]
            mapM_ (\(a, e) -> fromEcef e wgs84 `shouldBe` a) (zip refAngular refEcefs)
    describe "Spherical transformation between coordinates systems" $ do
        it "transforms NVector position to ECEF position" $ do
            let p = nvector 0.5 0.5 0.7071
            toEcef p s84 `shouldBe` ecefMetres 3185519.6603 3185519.6603 4504961.9036
        it "transforms angular position to ECEF position" $ do
            let refAngular =
                    [ decimalLatLongHeight 39.379 (-48.013) (metres 4702059.834)
                    , decimalLatLongHeight 45.0 45.0 zero
                    , decimalLatLongHeight 48.8562 2.3508 (metres 67.36972232195099)
                    , latLongHeight (readLatLong "531914N0014347W") (metres 15000.0)
                    , decimalLatLongHeight 53.1882691 0.1332744 (metres 15000.0)
                    ]
            let refEcefs =
                    [ ecefMetres 5725717.3542 (-6361955.6232) 7025277.9139
                    , ecefMetres 3185504.3857 3185504.3857 4504983.5053
                    , ecefMetres 4188328.8913 171940.276 4797806.6692
                    , ecefMetres 3812864.0945 (-115142.8631) 5121515.1612
                    , ecefMetres 3826406.4644 8900.5354 5112694.2386
                    ]
            mapM_ (\(a, e) -> toEcef a s84 `shouldBe` e) (zip refAngular refEcefs)
        it "transforms ECEF position to angular position" $ do
            let refAngular =
                    [ decimalLatLongHeight 39.379 (-48.013) (metres 4702059.8338)
                    , decimalLatLongHeight 45.0 45.0 (metres 1e-4)
                    , decimalLatLongHeight 48.8562 2.3508 (metres 67.3693)
                    ]
            let refEcefs =
                    [ ecefMetres 5725717.354 (-6361955.623) 7025277.914
                    , ecefMetres 3185504.386 3185504.386 4504983.505
                    , ecefMetres 4188328.891 171940.276 4797806.669
                    ]
            mapM_ (\(a, e) -> fromEcef e s84 `shouldBe` a) (zip refAngular refEcefs)
