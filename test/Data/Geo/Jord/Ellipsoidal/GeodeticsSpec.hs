module Data.Geo.Jord.Ellipsoidal.GeodeticsSpec
    ( spec
    ) where

-- import Data.Geo.Jord
import Prelude hiding (length)
import Test.Hspec

spec :: Spec
spec =
  describe "TODO" $ do
      it "should be re-enabled ;-)" $
          True `shouldBe` True
    -- describe "delta" $
    --     it "computes NED Vector between given position" $ do
    --         let p1 = geodeticPosition (latLongDecimal 49.66618 3.45063) 0 wgs84
    --         let p2 = geodeticPosition (latLongDecimal 48.88667 2.37472) 0 wgs84
    --         let d = exactDelta p1 p2
    --         d `shouldBe` nedVector (metres (-86125.881)) (metres (-78900.088)) (metres 1069.198)
