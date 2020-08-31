module Data.Geo.Jord.LocalFramesSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.LocalFrames as LocalFrames
import Data.Geo.Jord.Math3d (V3(..))

spec :: Spec
spec = do
    describe "Ellipsoidal earth model" $ do
        describe "destination" $ do
            it "return the given position if NED norm = 0" $ do
                let p0 = Geodetic.wgs84Pos 53.320556 (-1.729722) Length.zero
                let d = LocalFrames.Ned Length.zero Length.zero Length.zero
                LocalFrames.destinationN p0 d `shouldBe` p0
            it "computes the destination position from p0 and NED" $ do
                let p0 = Geodetic.wgs84Pos 49.66618 3.45063 Length.zero
                let d = LocalFrames.nedMetres (-86126) (-78900) 1069
                LocalFrames.destinationN p0 d `shouldBe`
                    Geodetic.wgs84Pos 48.886668961666665 2.3747212533333335 (Length.metres 0.198937)
            it "computes the destination position from p0 and vector in Frame B" $ do
                let p0 = Geodetic.wgs84Pos 49.66618 3.45063 Length.zero
                let y = Angle.decimalDegrees 10 -- yaw
                let r = Angle.decimalDegrees 20 -- roll
                let p = Angle.decimalDegrees 30 -- pitch
                let d = LocalFrames.deltaMetres 3000 2000 100
                LocalFrames.destination p0 (LocalFrames.frameB y r p) d `shouldBe`
                    Geodetic.wgs84Pos 49.69180157805555 3.4812670616666668 (Length.metres 6.007735)
        describe "nedBetween" $ do
            it "computes NED between surface positions" $ do
                let p1 = Geodetic.wgs84Pos 49.66618 3.45063 Length.zero
                let p2 = Geodetic.wgs84Pos 48.88667 2.37472 Length.zero
                let d = LocalFrames.nedBetween p1 p2
                d `shouldBe` LocalFrames.nedMetres (-86125.880549) (-78900.087818) 1069.19844
            it "computes NED between positions" $ do
                let p1 = Geodetic.wgs84Pos 49.66618 3.45063 (Length.metres 12000)
                let p2 = Geodetic.wgs84Pos 48.88667 2.37472 (Length.metres 15000)
                let d = LocalFrames.nedBetween p1 p2
                d `shouldBe` LocalFrames.nedMetres (-86328.623924) (-79085.290891) (-1928.287847)
        describe "deltaBetween" $
            it "computes delta between positions in frame L" $ do
                let p1 = Geodetic.wgs84Pos 1 2 (Length.metres (-3))
                let p2 = Geodetic.wgs84Pos 4 5 (Length.metres (-6))
                let w = Angle.decimalDegrees 5 -- wander azimuth
                let d = LocalFrames.deltaBetween p1 p2 (LocalFrames.frameL w)
                d `shouldBe` LocalFrames.deltaMetres 359490.578214 302818.522536 17404.271362
        describe "deltaBetween and destination consistency" $
            it "computes targetN p1 (nedBetween p1 p2) = p2" $ do
                let p1 = Geodetic.wgs84Pos 49.66618 3.45063 Length.zero
                let p2 = Geodetic.wgs84Pos 48.88667 2.37472 Length.zero
                LocalFrames.destinationN p1 (LocalFrames.nedBetween p1 p2) `shouldBe` p2
        describe "rotation matrix to/from earth-fixed frame" $ do
            it "computes rEN (frame N to earth-fixed frame)" $ do
                let p = Geodetic.wgs84Pos 49.66618 3.45063 Length.zero
                let f = LocalFrames.frameN p
                LocalFrames.rEF f `shouldBe`
                    [ V3 (-0.7609044147683025) (-6.018845511258954e-2) (-0.646066421861767)
                    , V3 (-4.5880841733693466e-2) 0.9981870315082038 (-3.895636649699221e-2)
                    , V3 0.6472398473115779 3.6854799449628455e-18 (-0.7622864160222752)
                    ]
            it "computes rEB (frame B to earth-fixed frame)" $ do
                let p = Geodetic.wgs84Pos 49.66618 3.45063 Length.zero
                let f =
                        LocalFrames.frameB
                            (Angle.decimalDegrees 10)
                            (Angle.decimalDegrees 20)
                            (Angle.decimalDegrees 30)
                            p
                LocalFrames.rEF f `shouldBe`
                    [ V3 (-0.49300713580470057) (-0.37038991706707025) (-0.7872453705044535)
                    , V3 0.1337450488624887 0.8618333991596926 (-0.4892396692804258)
                    , V3 0.8596837941652826 (-0.3464888186188679) (-0.375352198104241)
                    ]
    describe "North, East, Down delta" $ do
        describe "slantRange" $
            it "computes the slant range of a NED vector" $
            LocalFrames.slantRange (LocalFrames.nedMetres (-86126) (-78900) 1069) `shouldBe`
            Length.metres 116807.707952
        describe "bearing" $
            it "computes the bearing of a NED vector" $
            LocalFrames.bearing (LocalFrames.nedMetres (-86126) (-78900) 1069) `shouldBe`
            Angle.decimalDegrees 222.49278897666667
        describe "elevation" $
            it "computes the elevation of a NED vector from horizontal" $
            LocalFrames.elevation (LocalFrames.nedMetres (-86126) (-78900) 1069) `shouldBe`
            Angle.decimalDegrees (-0.5243664513888889)
