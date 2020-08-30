module Data.Geo.Jord.GeodeticSpec
    ( spec
    ) where

import Data.Maybe (mapMaybe)

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Models (GRS80(..), Mars2000(..), S84(..), WGS84(..))

spec :: Spec
spec = do
    describe "antipode" $ do
        it "returns the antipodal position" $ do
            Geodetic.antipode (Geodetic.wgs84Pos 45 154 (Length.metres 15000)) `shouldBe`
                Geodetic.wgs84Pos (-45) (-26) (Length.metres 15000)
            Geodetic.antipode (Geodetic.s84Pos 45 154 (Length.metres 15000)) `shouldBe`
                Geodetic.s84Pos (-45) (-26) (Length.metres 15000)
        it "returns the south pole when called with the north pole" $
            -- just check that latitude is -90; longitude will be 180 but does not matter
            -- at the pole
         do
            Geodetic.latitude (Geodetic.antipode (Geodetic.northPole WGS84)) `shouldBe`
                Angle.decimalDegrees (-90)
        it "returns the north pole when called with the south pole" $
            -- just check that latitude is 90; longitude will be 180 but does not matter
            -- at the pole
         do
            Geodetic.latitude (Geodetic.antipode (Geodetic.southPole WGS84)) `shouldBe`
                Angle.decimalDegrees 90
    describe "poles" $ do
        it "returns 90, 0 for the north pole" $ do
            Geodetic.latitude (Geodetic.northPole WGS84) `shouldBe` Angle.decimalDegrees 90
            Geodetic.longitude (Geodetic.northPole WGS84) `shouldBe` Angle.zero
        it "returns -90, 0 for the south pole" $ do
            Geodetic.latitude (Geodetic.southPole WGS84) `shouldBe` Angle.decimalDegrees (-90)
            Geodetic.longitude (Geodetic.southPole WGS84) `shouldBe` Angle.zero
    describe "wrapping latitude/longitude" $ do
        it "wraps a Earth position to [-90°, 90°] and [-180°, 180°]" $ do
            let p = Geodetic.s84Pos 91 54 Length.zero
            Geodetic.latitude p `shouldBe` Angle.decimalDegrees 89
            Geodetic.longitude p `shouldBe` Angle.decimalDegrees (-126)
        it "wraps a Mars position to [-90°, 90°] and [0°, 360°]" $ do
            let p = Geodetic.latLongPos 91 (-150) Mars2000
            Geodetic.latitude p `shouldBe` Angle.decimalDegrees 89
            Geodetic.longitude p `shouldBe` Angle.decimalDegrees 210
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
                    [ Geodetic.wgs84Pos 55.60583333333334 13.000555555555556 Length.zero
                    , Geodetic.wgs84Pos 55.6 13.0 Length.zero
                    , Geodetic.wgs84Pos 55.0 13.0 Length.zero
                    , Geodetic.wgs84Pos (-1.2830555555555556) 36.81666666666667 Length.zero
                    , Geodetic.wgs84Pos (-1.2666666666666666) 36.81666666666667 Length.zero
                    , Geodetic.wgs84Pos (-1.0) 36.0 Length.zero
                    , Geodetic.wgs84Pos 47.60611111111111 (-122.33194444444445) Length.zero
                    , Geodetic.wgs84Pos 47.6 (-122.31666666666666) Length.zero
                    , Geodetic.wgs84Pos 47.0 (-122.0) Length.zero
                    , Geodetic.wgs84Pos (-54.801944444444445) (-68.30305555555556) Length.zero
                    , Geodetic.wgs84Pos (-54.8) (-68.3) Length.zero
                    , Geodetic.wgs84Pos (-54.0) (-68.0) Length.zero
                    , Geodetic.wgs84Pos 55.60583333333334 13.000555555555556 Length.zero
                    , Geodetic.wgs84Pos (-1.2666666666666666) 36.81666666666667 Length.zero
                    , Geodetic.wgs84Pos 47.0 (-122.0) Length.zero
                    ]
            mapMaybe (`Geodetic.read` WGS84) texts `shouldBe` positions
        it "reads positions around the WGS84 ellipsoid" $ do
            let texts = ["55°36'21''N 013°00'02''E 5m", "55°36'21''N 013°00'02''E -5m"]
            let positions =
                    [ Geodetic.wgs84Pos 55.60583333333334 13.000555555555556 (Length.metres 5)
                    , Geodetic.wgs84Pos 55.60583333333334 13.000555555555556 (Length.metres (-5))
                    ]
            mapMaybe (`Geodetic.read` WGS84) texts `shouldBe` positions
        it "reads positions around the S84 sphere" $ do
            let texts = ["55°36'21''N 013°00'02''E 5m", "55°36'21''N 013°00'02''E -5m"]
            let positions =
                    [ Geodetic.s84Pos 55.60583333333334 13.000555555555556 (Length.metres 5)
                    , Geodetic.s84Pos 55.60583333333334 13.000555555555556 (Length.metres (-5))
                    ]
            mapMaybe (`Geodetic.read` S84) texts `shouldBe` positions
        it "reads Mars surface positions" $ do
            let texts = ["54S360E", "55°36'21''N 341°34'02''E"]
            let positions =
                    [ Geodetic.latLongPos (-54.0) 360 Mars2000
                    , Geodetic.latLongPos 55.60583333333334 341.5672222222222 Mars2000
                    ]
            mapMaybe (`Geodetic.read` Mars2000) texts `shouldBe` positions
    describe "Attempting to read invalid DMS text" $ do
        it "fails to read syntactically invalid positions" $ do
            let texts = ["553621K0130002E", "011659S0364900Z", "4736221221955W", "54480S0681811W"]
            mapMaybe (`Geodetic.read` WGS84) texts `shouldBe` []
        it "fails to read invalid WGS84 surface positions" $ do
            let texts = ["914807S0681811W", "804807S1811811W"]
            mapMaybe (`Geodetic.read` WGS84) texts `shouldBe` []
        it "fails to read invalid Mars surface positions" $ do
            let texts = ["914807S0681811E", "5448S06818W"]
            mapMaybe (`Geodetic.read` Mars2000) texts `shouldBe` []
    describe "Showing positions" $ do
        it "shows the N/E position formatted in DMS with symbols" $
            show (Geodetic.wgs84Pos 55.6058333333 13.00055556 (Length.metres 5)) `shouldBe`
            "55°36'21.000\"N,13°0'2.000\"E 5.0m (WGS84)"
        it "shows the S/E position formatted in DMS with symbols" $
            show (Geodetic.latLongPos (-1.28305556) 36.81666 GRS80) `shouldBe`
            "1°16'59.000\"S,36°48'59.976\"E 0.0m (GRS80)"
        it "shows the N/W position formatted in DMS with symbols" $
            show (Geodetic.latLongPos 47.60611 (-122.33194) S84) `shouldBe`
            "47°36'21.996\"N,122°19'54.984\"W 0.0m (S84)"
        it "shows the S/W position formatted in DMS with symbols" $
            show (Geodetic.latLongPos (-54.80194) (-68.30305) S84) `shouldBe`
            "54°48'6.984\"S,68°18'10.980\"W 0.0m (S84)"
