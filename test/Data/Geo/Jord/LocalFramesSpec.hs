module Data.Geo.Jord.LocalFramesSpec
    ( spec
    ) where

import Test.Hspec

import Data.Geo.Jord

spec :: Spec
spec = do
    describe "Ellipsoidal earth model" $ do
        describe "target" $ do
            it "return the given position if NED norm = 0" $ do
                let p0 = wgs84Pos 53.320556 (-1.729722) zero
                let d = ned zero zero zero
                targetN p0 d `shouldBe` p0
            it "computes the target position from p0 and NED" $ do
                let p0 = wgs84Pos 49.66618 3.45063 zero
                let d = nedMetres (-86126) (-78900) 1069
                targetN p0 d `shouldBe` wgs84Pos 48.886668961666665 2.3747212533333335 (metres 0.198937)
            it "computes the target position from p0 and vector in Frame B" $ do
                let p0 = wgs84Pos 49.66618 3.45063 zero
                let y = decimalDegrees 10 -- yaw
                let r = decimalDegrees 20 -- roll
                let p = decimalDegrees 30 -- pitch
                let d = deltaMetres 3000 2000 100
                target p0 (frameB y r p) d `shouldBe` wgs84Pos 49.69180157805555 3.4812670616666668 (metres 6.007736)
        describe "nedBetween" $ do
            it "computes NED between surface positions" $ do
                let p1 = wgs84Pos 49.66618 3.45063 zero
                let p2 = wgs84Pos 48.88667 2.37472 zero
                let d = nedBetween p1 p2
                d `shouldBe` nedMetres (-86125.880548) (-78900.087817) 1069.19844
            it "computes NED between positions" $ do
                let p1 = wgs84Pos 49.66618 3.45063 (metres 12000)
                let p2 = wgs84Pos 48.88667 2.37472 (metres 15000)
                let d = nedBetween p1 p2
                d `shouldBe` nedMetres (-86328.623924) (-79085.290891) (-1928.287848)
        describe "deltaBetween" $
            it "computes delta between positions in frame L" $ do
                let p1 = wgs84Pos 1 2 (metres (-3))
                let p2 = wgs84Pos 4 5 (metres (-6))
                let w = decimalDegrees 5 -- wander azimuth
                let d = deltaBetween p1 p2 (frameL w)
                d `shouldBe` deltaMetres 359490.578214 302818.522536 17404.271362
        describe "deltaBetween and target consistency" $
            it "computes targetN p1 (nedBetween p1 p2) = p2" $ do
                let p1 = wgs84Pos 49.66618 3.45063 zero
                let p2 = wgs84Pos 48.88667 2.37472 zero
                targetN p1 (nedBetween p1 p2) `shouldBe` p2
        describe "rotation matrix to/from earth-fixed frame" $ do
            it "computes rEN (frame N to earth-fixed frame)" $ do
                let p = wgs84Pos 49.66618 3.45063 zero
                let f = frameN p
                rEF f `shouldBe`
                    [ Vector3d (-0.7609044147683025) (-6.018845511258954e-2) (-0.646066421861767)
                    , Vector3d (-4.5880841733693466e-2) 0.9981870315082038 (-3.895636649699221e-2)
                    , Vector3d 0.6472398473115779 0.0 (-0.7622864160222752)
                    ]
            it "computes rEB (frame B to earth-fixed frame)" $ do
                let p = wgs84Pos 49.66618 3.45063 zero
                let f = frameB (decimalDegrees 10) (decimalDegrees 20) (decimalDegrees 30) p
                rEF f `shouldBe`
                    [ Vector3d (-0.49300713580470057) (-0.37038991706707025) (-0.7872453705044535)
                    , Vector3d 0.1337450488624887 0.8618333991596926 (-0.4892396692804258)
                    , Vector3d 0.8596837941652826 (-0.3464888186188679) (-0.375352198104241)
                    ]
    describe "North, East, Down delta" $ do
        describe "slantRange" $
            it "computes the slant range of a NED vector" $
            slantRange (nedMetres (-86126) (-78900) 1069) `shouldBe` metres 116807.707952
        describe "bearing" $
            it "computes the bearing of a NED vector" $
            bearing (nedMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees 222.49278897666667
        describe "elevation" $
            it "computes the elevation of a NED vector from horizontal" $
            elevation (nedMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees (-0.5243664513888889)
