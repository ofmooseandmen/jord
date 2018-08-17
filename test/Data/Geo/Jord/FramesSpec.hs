module Data.Geo.Jord.FramesSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Ellipsoidal earth model" $ do
        describe "target" $ do
            it "return the given point if NED norm = 0" $ do
                let p0 = readLatLong "531914N0014347W"
                let d = ned zero zero zero
                targetN p0 d wgs84 `shouldBe` p0
            it "computes the target point from p0 and NED" $ do
                let p0 = decimalLatLong 49.66618 3.45063
                let d = nedMetres (-86126) (-78900) 1069
                targetN p0 d wgs84 `shouldBe` decimalLatLong 48.8866688 2.374721388
            it "computes the target point from p0 and vector in Frame B" $ do
                let p0 = decimalLatLongHeight 49.66618 3.45063 zero
                let y = decimalDegrees 10 -- yaw
                let r = decimalDegrees 20 -- roll
                let p = decimalDegrees 30 -- pitch
                let d = deltaMetres 3000 2000 100
                target p0 (frameB y r p) d wgs84 `shouldBe`
                    decimalLatLongHeight 49.6918016 3.4812669 (metres 6.0077)
        describe "nedBetween" $ do
            it "computes NED between LatLong positions" $ do
                let p1 = decimalLatLong 49.66618 3.45063
                let p2 = decimalLatLong 48.88667 2.37472
                let d = nedBetween p1 p2 wgs84
                d `shouldBe` nedMetres (-86125.8805) (-78900.0878) 1069.1984
            it "computes NED between angular positions" $ do
                let p1 = decimalLatLongHeight 49.66618 3.45063 zero
                let p2 = decimalLatLongHeight 48.88667 2.37472 zero
                let d = nedBetween p1 p2 wgs84
                d `shouldBe` nedMetres (-86125.8805) (-78900.0878) 1069.1984
        describe "deltaBetween" $
            it "computes delta between angular positions in frame L" $ do
                let p1 = decimalLatLongHeight 1 2 (metres (-3))
                let p2 = decimalLatLongHeight 4 5 (metres (-6))
                let w = decimalDegrees 5 -- wander azimuth
                let d = deltaBetween p1 p2 (frameL w) wgs84
                d `shouldBe` deltaMetres 359490.5782 302818.5226 17404.2713
        describe "deltaBetween and target consistency" $
            it "computes targetN p1 (nedBetween p1 p2) = p2" $ do
                let p1 = decimalLatLongHeight 49.66618 3.45063 zero
                let p2 = decimalLatLongHeight 48.88667 2.37472 zero
                targetN p1 (nedBetween p1 p2 wgs84) wgs84 `shouldBe` p2
        describe "rotation matrix to go from/to earth-fixed frame to/from frame" $ do
            it "computes the rotation matrix to go from Frame N to earth-fixed frame" $ do
                let p = decimalLatLong 49.66618 3.45063
                let f = frameN p wgs84
                rEF f `shouldBe`
                    [ Vector3d (-0.7609044147650337) (-6.0188455103478165e-2) (-0.6460664218664659)
                    , Vector3d (-4.588084172652564e-2) 0.9981870315087531 (-3.8956366491356864e-2)
                    , Vector3d 0.6472398473159291 0.0 (-0.7622864160185809)
                    ]
            it "computes the rotation matrix to go from Frame B to earth-fixed frame" $ do
                let p = decimalLatLong 49.66618 3.45063
                let f = frameB (decimalDegrees 10) (decimalDegrees 20) (decimalDegrees 30) p wgs84
                rEF f `shouldBe`
                    [ Vector3d (-0.4930071357985816) (-0.3703899170611777) (-0.787245370511058)
                    , Vector3d 0.13374504886728417 0.8618333991629544 (-0.4892396692733688)
                    , Vector3d 0.8596837941680457 (-0.3464888186170537) (-0.37535219809958753)
                    ]
    describe "North, East, Down delta" $ do
        describe "slantRange" $
            it "computes the slant range of a NED vector" $
            slantRange (nedMetres (-86126) (-78900) 1069) `shouldBe` metres 116807.708
        describe "bearing" $
            it "computes the bearing of a NED vector" $
            bearing (nedMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees 222.4927888
        describe "elevation" $
            it "computes the elevation of a NED vector from horizontal" $
            elevation (nedMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees (-0.5243663)
