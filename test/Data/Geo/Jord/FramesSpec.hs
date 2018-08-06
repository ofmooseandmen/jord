module Data.Geo.Jord.FramesSpec
    ( spec
    ) where

import Data.Geo.Jord
import Test.Hspec

spec :: Spec
spec = do
    describe "Ellipsoidal earth model" $ do
        describe "target" $ do
            it "return the given point if NED vector vnorm = 0" $ do
                let p0 = readLatLong "531914N0014347W"
                let d = ned zero zero zero
                targetN p0 d wgs84 `shouldBe` p0
            it "computes the target point from p0 and NED vector" $ do
                let p0 = decimalLatLong 49.66618 3.45063
                let d = nedMetres (-86126) (-78900) 1069
                targetN p0 d wgs84 `shouldBe` decimalLatLong 48.8866688 2.374721111
        describe "deltaBetween" $ do
            it "computes NED Vector between LatLong positions" $ do
                let p1 = decimalLatLong 49.66618 3.45063
                let p2 = decimalLatLong 48.88667 2.37472
                let d = deltaBetween p1 p2 wgs84
                d `shouldBe` nedMetres (-86125.88049540376) (-78900.08718759022) 1069.1981930266265
            it "computes NED Vector between angular positions" $ do
                let p1 = decimalLatLongHeight 49.66618 3.45063 zero
                let p2 = decimalLatLongHeight 48.88667 2.37472 zero
                let d = deltaBetween p1 p2 wgs84
                d `shouldBe` nedMetres (-86125.88049540376) (-78900.08718759022) 1069.1981930266265
        describe "delta and target consistency" $
            it "computes target p1 (delta p1 p2) = p2" $ do
                let p1 = decimalLatLongHeight 49.66618 3.45063 zero
                let p2 = decimalLatLongHeight 48.88667 2.37472 zero
                targetN p1 (deltaBetween p1 p2 wgs84) wgs84 `shouldBe` p2
        describe "FrameB rotation matrix" $
            it "computes the rotation matrix to go from B to E" $ do
                let p = decimalLatLong 49.66618 3.45063
                let f = frameB (decimalDegrees 10) (decimalDegrees 20) (decimalDegrees 30) p wgs84
                frameToEarth f `shouldBe`
                    [ Vector3d (-0.49300714) (-0.37038992) (-0.78724537)
                    , Vector3d 0.13374505 0.8618334 (-0.48923967)
                    , Vector3d 0.85968379 (-0.34648882) (-0.3753522)
                    ]
    describe "North, East, Down delta" $ do
        describe "norm" $
            it "computes the norm of a NED vector" $
            norm (nedMetres (-86126) (-78900) 1069) `shouldBe` metres 116807.708
        describe "bearing" $
            it "computes the bearing of a NED vector" $
            bearing (nedMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees 222.4927888
        describe "elevation" $
            it "computes the elevation of a NED vector from horizontal" $
            elevation (nedMetres (-86126) (-78900) 1069) `shouldBe` decimalDegrees (-0.5243663)
