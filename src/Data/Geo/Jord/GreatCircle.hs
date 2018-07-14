-- |
-- Module:      Data.Geo.Jord.GreatCircle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with <https://en.wikipedia.org/wiki/Great_circle Great Circle>.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- This module assumes a spherical earth.
--
-- TODO:
--
--     * alongTrackDistance :: Position -> GreatArc -> Length
--
--     * intersection :: GreatArc -> GreatArc -> Maybe Position
--
--     * isWithin :: [Position] -> Bool
--
--     * nearestPoint :: Position -> GreatArc -> Position
--
--     * area :: [Position] -> Surface
--
--     * closestApproach
--
module Data.Geo.Jord.GreatCircle
    (
    -- * The 'GreatCircle' type
      GreatCircle
    -- * The 'Position' type
    , Position(..)
    -- * Smart constructors
    , greatCircle
    , greatCircleE
    , greatCircleF
    , greatCircleBearing
    -- * Geodesic calculations
    , antipode
    , crossTrackDistance
    , crossTrackDistance'
    , destination
    , destination'
    , distance
    , distance'
    , finalBearing
    , initialBearing
    , interpolate
    , intersections
    , meanEarthRadius
    , midpoint
    -- * Remarkable positions
    , northPole
    , southPole
    ) where

import Control.Monad.Fail
import Data.Geo.Jord.Angle
import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Maybe
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

-- | The 'Position' class defines 2 functions to convert a position to and from a 'NVector'.
-- All functions in this module first convert 'Position' to 'NVector' and any resulting 'NVector' back
-- to a 'Position'. This allows the call site to pass either 'NVector' or 'GeoPos' and to get back
-- the same class instance.
class Position a where
    -- | Converts a 'NVector' into 'Position' instance.
    fromNVector :: NVector -> a
    -- | Converts the 'Position' instance into a 'NVector'.
    toNVector :: a -> NVector

-- | 'GeoPos' to/from 'NVector'.
instance Position GeoPos where
    fromNVector v = latLong lat lon
      where
        lat = atan2' (z v) (sqrt (x v * x v + y v * y v))
        lon = atan2' (y v) (x v)
    toNVector g = nvector x' y' z'
      where
        lat = latitude g
        lon = longitude g
        cl = cos' lat
        x' = cl * cos' lon
        y' = cl * sin' lon
        z' = sin' lat

-- | Identity.
instance Position NVector where
    fromNVector v = v
    toNVector v = v

-- | 'GreateCircle' passing by both given 'Position's. 'error's if given positions are
-- equal or antipodal.
greatCircle :: (Eq a, Position a, Show a) => a -> a -> GreatCircle
greatCircle p1 p2 =
    fromMaybe
        (error (show p1 ++ " and " ++ show p2 ++ " do not define a unique Great Circle"))
        (greatCircleF p1 p2)

-- | 'GreateCircle' passing by both given 'Position's. A 'Left' indicates that given positions are
-- equal or antipodal.
greatCircleE :: (Eq a, Position a) => a -> a -> Either String GreatCircle
greatCircleE p1 p2
    | p1 == p2 = Left "Invalid Great Circle: positions are equal"
    | p1 == antipode p2 = Left "Invalid Great Circle: positions are antipodal"
    | otherwise =
        Right
            (GreatCircle
                 (cross v1 v2)
                 ("passing by " ++
                  show (fromNVector v1 :: GeoPos) ++ " & " ++ show (fromNVector v2 :: GeoPos)))
  where
    v1 = toNVector p1
    v2 = toNVector p2

-- | 'GreateCircle' passing by both given 'Position's. 'fail's if given positions are
-- equal or antipodal.
greatCircleF :: (Eq a, MonadFail m, Position a) => a -> a -> m GreatCircle
greatCircleF p1 p2 =
    case e of
        Left err -> fail err
        Right gc -> return gc
  where
    e = greatCircleE p1 p2

-- | 'GreatCircle' passing by the given 'Position' and heading on given bearing.
greatCircleBearing :: (Position a) => a -> Angle -> GreatCircle
greatCircleBearing p b =
    GreatCircle
        (sub n' e')
        ("passing by " ++ show (fromNVector v :: GeoPos) ++ " heading on " ++ show b)
  where
    v = toNVector p
    e = cross northPole v -- easting
    n = cross v e -- northing
    e' = scale e (cos' b / norm e)
    n' = scale n (sin' b / norm n)

-- | Returns the antipodal 'Position' of the given 'Position' - i.e. the position on the surface
-- of the Earth which is diametrically opposite to the given position.
antipode :: (Position a) => a -> a
antipode p = fromNVector (scale (toNVector p) (-1.0))

-- | 'crossTrackDistance'' assuming a radius of 'meanEarthRadius'.
crossTrackDistance :: (Position a) => a -> GreatCircle -> Length
crossTrackDistance p gc = crossTrackDistance' p gc meanEarthRadius

-- | Signed distance from given 'Position' to given 'GreatCircle'.
-- return a negative 'Length' if position if left of great circle,
-- positive 'Length' if position if right of great circle.
crossTrackDistance' :: (Position a) => a -> GreatCircle -> Length -> Length
crossTrackDistance' p gc =
    arcLength (sub (angleBetween (normal gc) (toNVector p) Nothing) (decimalDegrees 90))

-- | 'destination'' assuming a radius of 'meanEarthRadius'.
destination :: (Position a) => a -> Angle -> Length -> a
destination p b d = destination' p b d meanEarthRadius

-- | Computes the destination 'Position' from the given 'Position' having travelled the given distance on the
-- given initial bearing (bearing will normally vary before destination is reached) and using the given earth radius.
--
-- This is known as the direct geodetic problem.
destination' :: (Position a) => a -> Angle -> Length -> Length -> a
destination' p b d r
    | isZero d = p
    | otherwise = fromNVector (add (scale v (cos' ta)) (scale de (sin' ta)))
  where
    v = toNVector p
    ed = unit (cross northPole v) -- east direction vector at v
    nd = cross v ed -- north direction vector at v
    ta = central d r -- central angle
    de = add (scale nd (cos' b)) (scale ed (sin' b)) -- unit vector in the direction of the azimuth

-- | 'distance'' assuming a radius of 'meanEarthRadius'.
distance :: (Position a) => a -> a -> Length
distance p1 p2 = distance' p1 p2 meanEarthRadius

-- | Computes the surface distance (length of geodesic) in 'Meters' assuming a
-- spherical Earth between the two given 'Position's and using the given earth radius.
distance' :: (Position a) => a -> a -> Length -> Length
distance' p1 p2 = arcLength (angleBetween v1 v2 Nothing)
  where
    v1 = toNVector p1
    v2 = toNVector p2

-- | Computes the final bearing arriving at given destination  @p2@ 'Position' from given 'Position' @p1@.
--  the final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
-- Returns 180 if both position are equals.
finalBearing :: (Position a) => a -> a -> Angle
finalBearing p1 p2 = normalise (initialBearing p2 p1) (decimalDegrees 180)

-- | Computes the initial bearing from given @p1@ 'Position' to given @p2@ 'Position', in compass degrees.
-- Returns 0 if both position are equals.
initialBearing :: (Position a) => a -> a -> Angle
initialBearing p1 p2 = normalise (angleBetween gc1 gc2 (Just v1)) (decimalDegrees 360)
  where
    v1 = toNVector p1
    v2 = toNVector p2
    gc1 = cross v1 v2 -- great circle through p1 & p2
    gc2 = cross v1 northPole -- great circle through p1 & north pole

-- | Computes the 'Position' at given fraction @f@ between the two given 'Position's @p0@ and @p1@.
--
-- Special cases:
--
-- @
--     interpolate p0 p1 0.0 => p0
--     interpolate p0 p1 1.0 => p1
-- @
--
-- 'error's if @f < 0 || f > 1.0@
--
interpolate :: (Position a) => a -> a -> Double -> a
interpolate p0 p1 f
    | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
    | f == 0 = p0
    | f == 1 = p1
    | otherwise = fromNVector (unit (add v0 (scale (sub v1 v0) f)))
  where
    v0 = toNVector p0
    v1 = toNVector p1

-- | Computes the intersections between the two given 'GreatCircle's.
-- Two 'GreatCircle's intersect exactly twice unless there are equal, in which case 'Nothing' is returned.
intersections :: (Position a) => GreatCircle -> GreatCircle -> Maybe (a, a)
intersections gc1 gc2
    | norm i == 0.0 = Nothing
    | otherwise
    , let ni = unit i = Just (fromNVector ni, fromNVector (antipode ni))
  where
    i = cross (normal gc1) (normal gc2)

-- | Mean Earth radius: 6,371,008.8 metres.
meanEarthRadius :: Length
meanEarthRadius = metres 6371008.8

-- | Computes the mid 'Position' between the given list of 'Position's which must be non-empty.
midpoint :: (Position a) => [a] -> a
midpoint [] = error "midpoint expects a non-empty list"
midpoint [p] = p
midpoint ps = fromNVector (unit (foldl add zero vs))
  where
    vs = map toNVector ps

-- | 'Position' of the North Pole.
northPole :: (Position a) => a
northPole = fromNVector (nvector 0.0 0.0 1.0)

-- | 'Position' of the South Pole.
southPole :: (Position a) => a
southPole = fromNVector (nvector 0.0 0.0 (-1.0))

-- | Angle bteween the tow given 'NVector's.
-- If @n@ is 'Nothing', the angle is always in [0..180], otherwise it is in [-180, +180],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
angleBetween :: NVector -> NVector -> Maybe NVector -> Angle
angleBetween v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2
