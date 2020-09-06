module Data.Geo.Jord.RotationSpec
    ( spec
    ) where

import Test.Hspec

import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Math3d as Math3d
import qualified Data.Geo.Jord.Rotation as Rotation

spec :: Spec
spec = do
    describe "r2xyz" $
        it "computes the 3 angles about new axes in the xyz-order from rotation matrix" $ do
            let xyz = [Angle.decimalDegrees 45, Angle.decimalDegrees 45, Angle.decimalDegrees 5]
            let rm =
                    [ Math3d.vec3 0.7044160264027587 (-6.162841671621935e-2) 0.7071067811865475
                    , Math3d.vec3 0.559725765762092 0.6608381550289296 (-0.5)
                    , Math3d.vec3 (-0.43646893232965345) 0.7479938977765876 0.5000000000000001
                    ]
            Rotation.r2xyz rm `shouldBe` xyz
    describe "r2xyz" $
        it "computes the 3 angles about new axes in the zyx-order from rotation matrix" $ do
            let zyx = [Angle.decimalDegrees 10, Angle.decimalDegrees 20, Angle.decimalDegrees 30]
            let rm =
                    [ Math3d.vec3 0.9254165783983234 1.802831123629725e-2 0.37852230636979245
                    , Math3d.vec3 0.16317591116653482 0.8825641192593856 (-0.44096961052988237)
                    , Math3d.vec3 (-0.3420201433256687) 0.46984631039295416 0.8137976813493738
                    ]
            Rotation.r2zyx rm `shouldBe` zyx
    describe "xyz2r" $
        it "computes the rotation matrix from 3 angles about new axes in the xyz-order" $ do
            let x = Angle.decimalDegrees 45
            let y = Angle.decimalDegrees 45
            let z = Angle.decimalDegrees 5
            let rm =
                    [ Math3d.vec3 0.7044160264027587 (-6.162841671621935e-2) 0.7071067811865475
                    , Math3d.vec3 0.559725765762092 0.6608381550289296 (-0.5)
                    , Math3d.vec3 (-0.43646893232965345) 0.7479938977765876 0.5000000000000001
                    ]
            Rotation.xyz2r x y z `shouldBe` rm
    describe "zyx2r" $
        it "computes the rotation matrix from 3 angles about new axes in the zyx-order" $ do
            let x = Angle.decimalDegrees 10
            let y = Angle.decimalDegrees 20
            let z = Angle.decimalDegrees 30
            let rm =
                    [ Math3d.vec3 0.9254165783983234 1.802831123629725e-2 0.37852230636979245
                    , Math3d.vec3 0.16317591116653482 0.8825641192593856 (-0.44096961052988237)
                    , Math3d.vec3 (-0.3420201433256687) 0.46984631039295416 0.8137976813493738
                    ]
            Rotation.zyx2r x y z `shouldBe` rm
