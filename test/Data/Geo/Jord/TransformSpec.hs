module Data.Geo.Jord.TransformSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec =
    describe "Ellipsoidal transformation between coordinates systems" $ do
        it "transforms NVector position to ECEF position" $ do
            let p = ellipsoidal (nvector 0.5 0.5 0.7071) wgs84
            toEcef p `shouldBe` ellipsoidal (ecefPositionMetres 3194434.411029306 3194434.411029306 4487326.819249299) wgs84
        it "transforms angular position to ECEF position" $ do
            let refAngular =
                    [ ellipsoidal (elevatedLatLong (latLongDecimal 39.379 (-48.013)) 4702059.834) wgs84
                    , ellipsoidal (elevatedLatLong (latLongDecimal 45.0 45.0) 0) wgs84
                    , ellipsoidal (elevatedLatLong (latLongDecimal 48.8562 2.3508) 67.36972232195099) wgs84
                    ]
            let refEcefs =
                    [ ellipsoidal (ecefPositionMetres 5733855.77488171 (-6370998.38026088) 7008137.51062469) wgs84
                    , ellipsoidal (ecefPositionMetres 3194419.14512197 3194419.14512197 4487348.40860601) wgs84
                    , ellipsoidal (ecefPositionMetres 4200996.76974058 172460.32072401 4780102.80780980) wgs84
                    ]
            mapM_ (\(g, e) -> toEcef g `shouldBe` e) (zip refAngular refEcefs)
        it "transforms ECEF position to angular position" $ do
            let refAngular =
                    [ ellipsoidal (elevatedLatLong (latLongDecimal 39.379 (-48.013)) 4702059.834050887) wgs84
                    , ellipsoidal (elevatedLatLong (latLongDecimal 45.0 45.0) 3.423524451828947e-5) wgs84
                    , ellipsoidal (elevatedLatLong (latLongDecimal 48.8562 2.3508) 67.36990469945641) wgs84
                    ]
            let refEcefs =
                    [ ellipsoidal (ecefPositionMetres 5733855.774881717 (-6370998.380260889) 7008137.510624695) wgs84
                    , ellipsoidal (ecefPositionMetres 3194419.145121972 3194419.145121971 4487348.408606014) wgs84
                    , ellipsoidal (ecefPositionMetres 4200996.769831858 172460.320727757 4780102.807914356) wgs84
                    ]
            mapM_ (\(g, e) -> fromEcef e `shouldBe` g) (zip refAngular refEcefs)
