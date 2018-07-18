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
module Data.Geo.Jord.Spherical.GreatCircle
    (
    -- * The 'GreatCircle' type
      GreatCircle
    -- * Smart constructors
    , greatCircle
    , greatCircleE
    , greatCircleF
    , greatCircleBearing
    -- * Geodesic calculations
    , crossTrackDistance
    , crossTrackDistance'
    , intersections
    , isInside
    ) where

import Control.Monad.Fail
import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Position (Geodetic2D(..), northPole)
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Spherical.Geodetics
import Data.Maybe (fromMaybe)
import Prelude hiding (fail)

-- | A circle on the surface of the Earth which lies in a plane passing through
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

-- | 'GreatCircle' passing by both given horizontal positions. 'error's if given positions are
-- equal or antipodal.
greatCircle :: (Eq a, Geodetic2D a, Show a) => a -> a -> GreatCircle
greatCircle p1 p2 =
    fromMaybe
        (error (show p1 ++ " and " ++ show p2 ++ " do not define a unique Great Circle"))
        (greatCircleF p1 p2)

-- | 'GreatCircle' passing by both given horizontal positions. A 'Left' indicates that given positions are
-- equal or antipodal.
greatCircleE :: (Geodetic2D a) => a -> a -> Either String GreatCircle
greatCircleE p1 p2
    | p1 == p2 = Left "Invalid Great Circle: positions are equal"
    | p1 == antipode p2 = Left "Invalid Great Circle: positions are antipodal"
    | otherwise =
        Right
            (GreatCircle
                 (cross v1 v2)
                 ("passing by " ++
                  show (fromNVector v1 :: LatLong) ++ " & " ++ show (fromNVector v2 :: LatLong)))
  where
    v1 = toNVector p1
    v2 = toNVector p2

-- | 'GreatCircle' passing by both given horizontal positions. 'fail's if given positions are
-- equal or antipodal.
greatCircleF :: (MonadFail m, Geodetic2D a) => a -> a -> m GreatCircle
greatCircleF p1 p2 =
    case e of
        Left err -> fail err
        Right gc -> return gc
  where
    e = greatCircleE p1 p2

-- | 'GreatCircle' passing by the given horizontal position and heading on given bearing.
greatCircleBearing :: (Geodetic2D a) => a -> Angle -> GreatCircle
greatCircleBearing p b =
    GreatCircle
        (sub n' e')
        ("passing by " ++ show (fromNVector v :: LatLong) ++ " heading on " ++ show b)
  where
    v = toNVector p
    e = cross northPole v -- easting
    n = cross v e -- northing
    e' = scale e (cos' b / norm e)
    n' = scale n (sin' b / norm n)

-- | 'crossTrackDistance'' using the mean radius of the WGS84 reference ellipsoid.
crossTrackDistance :: (Geodetic2D a) => a -> GreatCircle -> Length
crossTrackDistance p gc = crossTrackDistance' p gc (meanRadius wgs84)

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
crossTrackDistance' :: (Geodetic2D a) => a -> GreatCircle -> Length -> Length
crossTrackDistance' p gc =
    arcLength (sub (angularDistance (normal gc) (toNVector p) Nothing) (decimalDegrees 90))

-- | Computes the intersections between the two given 'GreatCircle's.
-- Two 'GreatCircle's intersect exactly twice unless there are equal (regardless of orientation),
-- in which case 'Nothing' is returned.
intersections :: (Geodetic2D a) => GreatCircle -> GreatCircle -> Maybe (a, a)
intersections gc1 gc2
    | norm i == 0.0 = Nothing
    | otherwise
    , let ni = unit i = Just (fromNVector ni, fromNVector (antipode ni))
  where
    i = cross (normal gc1) (normal gc2)