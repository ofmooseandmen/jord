{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Data.Geo.Jord.Spherical.GreatCircle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with <https://en.wikipedia.org/wiki/Great_circle Great Circle> assuming a spherical earth model.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.GreatCircle
    ( GreatCircle
    , GreatCircleGeodetics(..)
    ) where

import Control.Monad.Fail
import Data.Fixed
import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Spherical
import Data.Geo.Jord.Transform
import Data.Maybe (fromMaybe)
import Prelude hiding (fail)

-- | A circle on the _surface_ of the Earth which lies in a plane passing through
-- the Earth's centre. Every two distinct and non-antipodal points on the surface
-- of the Earth define a Great Circle.
--
-- It is internally represented as its normal vector - i.e. the normal vector
-- to the plane containing the great circle.
--
-- See 'greatCircle', 'greatCircleE', 'greatCircleF' or 'greatCircleBearing' constructors.
--
data GreatCircle = GreatCircle
    { normal :: NVector
    , dscr :: String
    } deriving (Eq)

instance Show GreatCircle where
    show = dscr

-- | Geodetics calculations on 'GreatCircle' assuming a spherical earth model.
class (Eq a, Show a) => GreatCircleGeodetics a where
    -- | 'GreatCircle' passing by both given positions. 'error's if given positions are
    -- equal or antipodal.
    greatCircle :: a -> a -> GreatCircle
    greatCircle p1 p2 =
        fromMaybe
            (error (show p1 ++ " and " ++ show p2 ++ " do not define a unique Great Circle"))
            (greatCircleF p1 p2)
    -- | 'GreatCircle' passing by both given positions. A 'Left' indicates that given positions are
    -- equal or antipodal.
    greatCircleE :: a -> a -> Either String GreatCircle
    -- | 'GreatCircle' passing by both given positions. 'fail's if given positions are
    -- equal or antipodal.
    greatCircleF :: (MonadFail m) => a -> a -> m GreatCircle
    greatCircleF p1 p2 =
        case e of
            Left err -> fail err
            Right gc -> return gc
      where
        e = greatCircleE p1 p2
    -- | 'GreatCircle' passing by the given position and heading on given bearing.
    greatCircleBearing :: a -> Angle -> GreatCircle
    -- | @crossTrackDistance' p gc@ computes the signed distance horizontal position @p@ to great circle @gc@.
    -- Returns a negative 'Length' if position if left of great circle,
    -- positive 'Length' if position if right of great circle; the orientation of the
    -- great circle is therefore important:
    --
    -- @
    --     let gc1 = greatCircle (latLongDecimal 51 0) (latLongDecimal 52 1)
    --     let gc2 = greatCircle (latLongDecimal 52 1) (latLongDecimal 51 0)
    --     crossTrackDistance p gc1 == (- crossTrackDistance p gc2)
    -- @
    crossTrackDistance :: a -> GreatCircle -> Length -> Length
    -- | Computes the intersections between the two given 'GreatCircle's.
    -- Two 'GreatCircle's intersect exactly twice unless there are equal (regardless of orientation),
    -- in which case 'Nothing' is returned.
    intersections :: GreatCircle -> GreatCircle -> Maybe (a, a)

-- | Great Circle geodetics on 'NVector's.
instance GreatCircleGeodetics NVector where
    greatCircleE v1 v2
        | v1 == v2 = Left "Invalid Great Circle: positions are equal"
        | (realToFrac (norm (add v1 v2) :: Double) :: Nano) == 0 =
            Left "Invalid Great Circle: positions are antipodal"
        | otherwise =
            Right
                (GreatCircle
                     (cross v1 v2)
                     ("passing by " ++
                      show (fromNVector v1 0.0 :: LatLong) ++
                      " & " ++ show (fromNVector v2 0.0 :: LatLong)))
    greatCircleBearing v b =
        GreatCircle
            (sub n' e')
            ("passing by " ++ show (fromNVector v 0.0 :: LatLong) ++ " heading on " ++ show b)
      where
        e = cross northPole v -- easting
        n = cross v e -- northing
        e' = scale e (cos' b / norm e)
        n' = scale n (sin' b / norm n)
    crossTrackDistance v gc =
        arcLength (sub (angularDistance (normal gc) v Nothing) (decimalDegrees 90))
    intersections gc1 gc2
        | (norm i :: Double) == 0.0 = Nothing
        | otherwise
        , let ni = unit i = Just (ni, antipode ni)
      where
        i = cross (normal gc1) (normal gc2)

-- | Great Circle geodetics on 'LatLong's.
instance GreatCircleGeodetics LatLong where
    greatCircleE p1 p2 = greatCircleE (toNVector p1) (toNVector p2)
    greatCircleBearing p = greatCircleBearing (toNVector p)
    crossTrackDistance p = crossTrackDistance (toNVector p)
    intersections gc1 gc2 = fmap (\(p1, p2) -> (fromNVector p1 0.0, fromNVector p2 0.0)) is
      where
        is = intersections gc1 gc2 :: Maybe (NVector, NVector)

-- | Great Circle geodetics on 'NVector' 'AngularPosition's.
instance GreatCircleGeodetics (AngularPosition NVector) where
    greatCircleE p1 p2 = greatCircleE (toNVector p1) (toNVector p2)
    greatCircleBearing p = greatCircleBearing (toNVector p)
    crossTrackDistance p = crossTrackDistance (toNVector p)
    intersections gc1 gc2 = fmap (\(p1, p2) -> (fromNVector p1 0.0, fromNVector p2 0.0)) is
      where
        is = intersections gc1 gc2 :: Maybe (NVector, NVector)

-- | Great Circle geodetics on 'LatLong' 'AngularPosition's.
instance GreatCircleGeodetics (AngularPosition LatLong) where
    greatCircleE p1 p2 = greatCircleE (toNVector p1) (toNVector p2)
    greatCircleBearing p = greatCircleBearing (toNVector p)
    crossTrackDistance p = crossTrackDistance (toNVector p)
    intersections gc1 gc2 = fmap (\(p1, p2) -> (fromNVector p1 0.0, fromNVector p2 0.0)) is
      where
        is = intersections gc1 gc2 :: Maybe (NVector, NVector)
