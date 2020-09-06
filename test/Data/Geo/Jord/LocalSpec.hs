module Data.Geo.Jord.LocalSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Local (Ned(..))
import qualified Data.Geo.Jord.Local as Local
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Models (WGS84(..))

spec :: Spec
spec = do
    describe "Ellipsoidal earth model" $ do
        describe "destination" $ do
            it "return the given position if NED norm = 0" $ do
                let p0 = Geodetic.latLongHeightPos 53.320556 (-1.729722) Length.zero WGS84
                let d = Ned Length.zero Length.zero Length.zero
                Local.destinationN p0 d `shouldBe` p0
            it "computes the destination position from p0 and NED" $ do
                let p0 = Geodetic.latLongHeightPos 49.66618 3.45063 Length.zero WGS84
                let d = Local.nedMetres (-86126) (-78900) 1069
                Local.destinationN p0 d `shouldBe`
                    Geodetic.latLongHeightPos
                        48.886668961666665
                        2.3747212533333335
                        (Length.metres 0.198937)
                        WGS84
            it "computes the destination position from p0 and vector in Frame B" $ do
                let p0 = Geodetic.latLongHeightPos 49.66618 3.45063 Length.zero WGS84
                let y = Angle.decimalDegrees 10 -- yaw
                let r = Angle.decimalDegrees 20 -- roll
                let p = Angle.decimalDegrees 30 -- pitch
                let d = Local.deltaMetres 3000 2000 100
                Local.destination p0 (Local.frameB y r p) d `shouldBe`
                    Geodetic.latLongHeightPos
                        49.69180157805555
                        3.4812670616666668
                        (Length.metres 6.007735)
                        WGS84
        describe "nedBetween" $ do
            it "computes NED between surface positions" $ do
                let p1 = Geodetic.latLongHeightPos 49.66618 3.45063 Length.zero WGS84
                let p2 = Geodetic.latLongHeightPos 48.88667 2.37472 Length.zero WGS84
                let d = Local.nedBetween p1 p2
                d `shouldBe` Local.nedMetres (-86125.880549) (-78900.087818) 1069.19844
            it "computes NED between positions" $ do
                let p1 = Geodetic.latLongHeightPos 49.66618 3.45063 (Length.metres 12000) WGS84
                let p2 = Geodetic.latLongHeightPos 48.88667 2.37472 (Length.metres 15000) WGS84
                let d = Local.nedBetween p1 p2
                d `shouldBe` Local.nedMetres (-86328.623924) (-79085.290891) (-1928.287847)
        describe "deltaBetween" $
            it "computes delta between positions in frame L" $ do
                let p1 = Geodetic.latLongHeightPos 1 2 (Length.metres (-3)) WGS84
                let p2 = Geodetic.latLongHeightPos 4 5 (Length.metres (-6)) WGS84
                let w = Angle.decimalDegrees 5 -- wander azimuth
                let d = Local.deltaBetween p1 p2 (Local.frameL w)
                d `shouldBe` Local.deltaMetres 359490.578214 302818.522536 17404.271362
        describe "deltaBetween and destination consistency" $
            it "computes targetN p1 (nedBetween p1 p2) = p2" $ do
                let p1 = Geodetic.latLongHeightPos 49.66618 3.45063 Length.zero WGS84
                let p2 = Geodetic.latLongHeightPos 48.88667 2.37472 Length.zero WGS84
                Local.destinationN p1 (Local.nedBetween p1 p2) `shouldBe` p2
        describe "rotation matrix to/from earth-fixed frame" $ do
            it "computes rEN (frame N to earth-fixed frame)" $ do
                let p = Geodetic.latLongHeightPos 0.0 0.0 Length.zero WGS84
                let f = Local.frameN p
                Local.rEF f `shouldBe`
                    [Math3d.vec3 0.0 0.0 (-1.0), Math3d.vec3 0.0 1.0 0.0, Math3d.vec3 1.0 0.0 0.0]
            it "computes rEB (frame B to earth-fixed frame)" $ do
                let p = Geodetic.latLongHeightPos 49.66618 3.45063 Length.zero WGS84
                let f =
                        Local.frameB
                            (Angle.decimalDegrees 10)
                            (Angle.decimalDegrees 20)
                            (Angle.decimalDegrees 30)
                            p
                Local.rEF f `shouldBe`
                    [ Math3d.vec3
                          (-0.49300713580470057)
                          (-0.37038991706707025)
                          (-0.7872453705044535)
                    , Math3d.vec3 0.1337450488624887 0.8618333991596926 (-0.4892396692804258)
                    , Math3d.vec3 0.8596837941652826 (-0.3464888186188679) (-0.375352198104241)
                    ]
    describe "North, East, Down delta" $ do
        describe "slantRange" $
            it "computes the slant range of a NED vector" $
            Local.slantRange (Local.nedMetres (-86126) (-78900) 1069) `shouldBe`
            Length.metres 116807.707952
        describe "bearing" $
            it "computes the bearing of a NED vector" $
            Local.bearing (Local.nedMetres (-86126) (-78900) 1069) `shouldBe`
            Angle.decimalDegrees 222.49278897666667
        describe "elevation" $
            it "computes the elevation of a NED vector from horizontal" $
            Local.elevation (Local.nedMetres (-86126) (-78900) 1069) `shouldBe`
            Angle.decimalDegrees (-0.5243664513888889)
