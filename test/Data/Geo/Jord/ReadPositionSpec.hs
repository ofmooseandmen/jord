module Data.Geo.Jord.ReadPositionSpec
    ( spec
    ) where

import Data.Maybe (mapMaybe)

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "Reading valid DMS text" $ do
        it "reads WGS84 surface positions" $ do
            let texts =
                    [ "553621N0130002E"
                    , "5536N01300E"
                    , "55N013E"
                    , "011659S0364900E"
                    , "0116S03649E"
                    , "01S036E"
                    , "473622N1221955W"
                    , "4736N12219W"
                    , "47N122W"
                    , "544807S0681811W"
                    , "5448S06818W"
                    , "54S068W"
                    , "55°36'21''N 013°00'02''E"
                    , "1°16'S,36°49'E"
                    , "47°N 122°W"
                    ]
            let positions =
                    [ wgs84Pos 55.60583333333334 13.000555555555556 zero
                    , wgs84Pos 55.6 13.0 zero
                    , wgs84Pos 55.0 13.0 zero
                    , wgs84Pos (-1.2830555555555556) 36.81666666666667 zero
                    , wgs84Pos (-1.2666666666666666) 36.81666666666667 zero
                    , wgs84Pos (-1.0) 36.0 zero
                    , wgs84Pos 47.60611111111111 (-122.33194444444445) zero
                    , wgs84Pos 47.6 (-122.31666666666666) zero
                    , wgs84Pos 47.0 (-122.0) zero
                    , wgs84Pos (-54.801944444444445) (-68.30305555555556) zero
                    , wgs84Pos (-54.8) (-68.3) zero
                    , wgs84Pos (-54.0) (-68.0) zero
                    , wgs84Pos 55.60583333333334 13.000555555555556 zero
                    , wgs84Pos (-1.2666666666666666) 36.81666666666667 zero
                    , wgs84Pos 47.0 (-122.0) zero
                    ]
            mapMaybe (`readPosition` WGS84) texts `shouldBe` positions
        it "reads positions around the WGS84 ellipsoid" $ do
            let texts = ["55°36'21''N 013°00'02''E 5m", "55°36'21''N 013°00'02''E -5m"]
            let positions =
                    [ wgs84Pos 55.60583333333334 13.000555555555556 (metres 5)
                    , wgs84Pos 55.60583333333334 13.000555555555556 (metres (-5))
                    ]
            mapMaybe (`readPosition` WGS84) texts `shouldBe` positions
        it "reads positions around the S84 sphere" $ do
            let texts = ["55°36'21''N 013°00'02''E 5m", "55°36'21''N 013°00'02''E -5m"]
            let positions =
                    [ s84Pos 55.60583333333334 13.000555555555556 (metres 5)
                    , s84Pos 55.60583333333334 13.000555555555556 (metres (-5))
                    ]
            mapMaybe (`readPosition` S84) texts `shouldBe` positions
        it "reads Mars surface positions" $ do
            let texts = ["54S360E", "55°36'21''N 341°34'02''E"]
            let positions = [latLongPos (-54.0) 360 Mars2000, latLongPos 55.60583333333334 341.5672222222222 Mars2000]
            mapMaybe (`readPosition` Mars2000) texts `shouldBe` positions
    describe "Attempting to read invalid DMS text" $ do
        it "fails to read syntactically invalid positions" $ do
            let texts = ["553621K0130002E", "011659S0364900Z", "4736221221955W", "54480S0681811W"]
            mapMaybe (`readPosition` WGS84) texts `shouldBe` []
        it "fails to read invalid WGS84 surface positions" $ do
            let texts = ["914807S0681811W", "804807S1811811W"]
            mapMaybe (`readPosition` WGS84) texts `shouldBe` []
        it "fails to read invalid Mars surface positions" $ do
            let texts = ["914807S0681811E", "5448S06818W"]
            mapMaybe (`readPosition` Mars2000) texts `shouldBe` []