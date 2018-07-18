module Data.Geo.Jord.PositionSpec
    ( spec
    ) where

import Data.Geo.Jord
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
                    [ ecefVector 5733855.774881717 (-6370998.380260889) 7008137.510624695
                    , ecefVector 3194419.145121972 3194419.145121971 4487348.408606014
                    , ecefVector 4200996.769740585 172460.32072401006 4780102.807809802
                    ]
            mapM_ (\(g, e) -> geodeticToEcef g wgs84 `shouldBe` e) (zip refGeodetics refEcefs)
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
