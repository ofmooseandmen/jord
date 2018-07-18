module Data.Geo.Jord.Spherical.GreatCircleSpec
    ( spec
    ) where

import Data.Geo.Jord
import Data.Maybe
import Test.Hspec

spec :: Spec
spec = do
    describe "Cross Track Distance" $ do
        it "returns a negative length when position is left of great circle (bearing)" $ do
            let p = latLongDecimal 53.2611 (-0.7972)
            let gc = greatCircleBearing (latLongDecimal 53.3206 (-1.7297)) (decimalDegrees 96.0)
            crossTrackDistance p gc `shouldBe` metres (-305.663)
        it "returns a negative length when position is left of great circle" $ do
            let p = latLongDecimal 53.2611 (-0.7972)
            let gc = greatCircle (latLongDecimal 53.3206 (-1.7297)) (latLongDecimal 53.1887 0.1334)
            crossTrackDistance p gc `shouldBe` metres (-307.547)
        it "returns a positve length when position is right of great circle (bearing)" $ do
            let p = readLatLong "531540N0014750W"
            let gc = greatCircleBearing (readLatLong "531914N0014347W") (readAngle "96d01m18s")
            crossTrackDistance p gc `shouldBe` metres 7042.324
        it "returns a positive length when position is left of great circle" $ do
            let p = antipode (latLongDecimal 53.2611 (-0.7972))
            let gc = greatCircle (latLongDecimal 53.3206 (-1.7297)) (latLongDecimal 53.1887 0.1334)
            crossTrackDistance p gc `shouldBe` metres 307.547
    describe "Intersections" $ do
        it "returns nothing if both great circle are equals" $ do
            let gc = greatCircleBearing (latLongDecimal 51.885 0.235) (decimalDegrees 108.63)
            (intersections gc gc :: Maybe (LatLong, LatLong)) `shouldBe` Nothing
        it "returns nothing if both great circle are equals (opposite orientation)" $ do
            let gc1 = greatCircle (latLongDecimal 51.885 0.235) (latLongDecimal 52.885 1.235)
            let gc2 = greatCircle (latLongDecimal 52.885 1.235) (latLongDecimal 51.885 0.235)
            (intersections gc1 gc2 :: Maybe (LatLong, LatLong)) `shouldBe` Nothing
        it "returns the two points where the two great circles intersects" $ do
            let gc1 = greatCircleBearing (latLongDecimal 51.885 0.235) (decimalDegrees 108.63)
            let gc2 = greatCircleBearing (latLongDecimal 49.008 2.549) (decimalDegrees 32.72)
            let (i1, i2) = fromJust (intersections gc1 gc2)
            i1 `shouldBe` latLongDecimal 50.9017226 4.4942782
            i2 `shouldBe` antipode i1
    describe "Great Circle Smart constructors" $ do
        it "fails if both positions are equal" $
            greatCircleE (latLongDecimal 3 154) (latLongDecimal 3 154) `shouldBe`
            Left "Invalid Great Circle: positions are equal"
        it "fails if both positions are antipodal" $
            greatCircleE (latLongDecimal 3 154) (antipode (latLongDecimal 3 154)) `shouldBe`
            Left "Invalid Great Circle: positions are antipodal"
