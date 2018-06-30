-- |
-- TODO add doc
-- TODO add crossTrackDistance Position Arc or Position Position Bearing
-- TODO add alongTrackDistance Position Arc or Position Position Bearing
-- TODO add initialBearing
-- TODO add intersection :: (Position a) => Arc -> Arc -> a
-- TODO add finalBearing
-- TODO add isWithin Position Arc
-- TODO add isWithin Position [Position]
-- TODO add nearestPointOnGreatCircle Position Arc
-- TODO add area [Position]
--
module Data.Geo.Jord.GreatCircle
    ( Arc(start, end)
    , Meters(..)
    , MetersPerSecond(..)
    , Millis(..)
    , Position
    , antipode
    , arc
    , destination
    , distance
    , interpolate
    , meanEarthRadius
    , midpoint
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.NVector
import Prelude hiding (subtract)

data Arc = Arc
    { start :: NVector
    , end :: NVector
    } deriving (Eq, Show)

newtype Meters = Meters
    { meters :: Double
    } deriving (Eq, Show)

newtype MetersPerSecond = MetersPerSecond
    { metersPerSecond :: Double
    } deriving (Eq, Show)

newtype Millis = Millis
    { millis :: Int
    } deriving (Eq, Show)

class Position a where
    fromNVector :: NVector -> a
    toNVector :: a -> NVector

instance Position GeoPos where
    fromNVector v = geo (toDegrees lat) (toDegrees lon)
      where
        lat = atan2 (z v) (sqrt (x v * x v + y v * y v))
        lon = atan2 (y v) (x v)
    toNVector g = nvector x' y' z'
      where
        lat = toRadians (latitude g)
        lon = toRadians (longitude g)
        cl = cos lat
        x' = cl * cos lon
        y' = cl * sin lon
        z' = sin lat

instance Position NVector where
    fromNVector v = v
    toNVector v = v

antipode :: (Position a) => a -> a
antipode p = fromNVector (scale (toNVector p) (-1.0))

arc :: (Position a) => a -> a -> Arc
arc p1 p2 = Arc (toNVector p1) (toNVector p2)

destination :: (Position a) => a -> Degrees -> Meters -> a
destination p b d = fromNVector (add (scale v (cos ta)) (scale de (sin ta)))
  where
    v = toNVector p
    ed = normalise (cross northPole v) -- east direction vector at v
    nd = cross v ed -- north direction vector at v
    a = toRadians b -- azimuth in radians
    ta = meters d / meters meanEarthRadius -- angle travelled in radians
    de = add (scale nd (cos a)) (scale ed (sin a)) -- unit vector in the direction of the azimuth

distance :: (Position a) => a -> a -> Meters
distance p1 p2 = Meters (meters meanEarthRadius * atan2 (norm (cross v1 v2)) (dot v1 v2))
  where
    v1 = toNVector p1
    v2 = toNVector p2

interpolate :: (Position a) => a -> Millis -> a -> Millis -> Millis -> a
interpolate p0 t0 p1 t1 ti = fromNVector (normalise (add v0 (scale (subtract v1 v0) s)))
  where
    v0 = toNVector p0
    v1 = toNVector p1
    s = fromIntegral (millis ti - millis t0) / fromIntegral (millis t1 - millis t0)

meanEarthRadius :: Meters
meanEarthRadius = Meters 6371008.8

midpoint :: (Position a) => [a] -> a
midpoint ps = fromNVector (normalise (foldl add zero vs))
  where
    vs = map toNVector ps

northPole :: (Position a) => a
northPole = fromNVector (nvector 0.0 0.0 1.0)

southPole :: (Position a) => a
southPole = fromNVector (nvector 0.0 0.0 (-1.0))

toRadians :: Degrees -> Double
toRadians d = degrees d * pi / 180.0

toDegrees :: Double -> Double
toDegrees r = r / pi * 180.0
