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
module Data.Geo.Jord
    ( Position
    , GeoPos
    , NVector
    , Degrees(..)
    , Meters(..)
    , MetersPerSecond(..)
    , Millis(..)
    , antipode
    , arc
    , destination
    , distance
    , geo
    , interpolate
    , meanEarthRadius
    , midpoint
    , north
    , nvector
    , south
    ) where

import Control.Applicative
import Data.Char
import Prelude hiding (subtract)
import Text.ParserCombinators.ReadP

data GeoPos = GeoPos
    { latitude :: Degrees
    , longitude :: Degrees
    } deriving (Eq, Show)

data NVector = NVector
    { x :: Double
    , y :: Double
    , z :: Double
    } deriving (Eq, Show)

data Arc = Arc
    { start :: NVector
    , end :: NVector
    } deriving (Eq, Show)

newtype Degrees = Degrees
    { degrees :: Double
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
    fromNVector v = GeoPos (toDegrees lat) (toDegrees lon)
      where
        lat = atan2 (z v) (sqrt (x v * x v + y v * y v))
        lon = atan2 (y v) (x v)
    toNVector g = NVector x' y' z'
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

instance Read GeoPos where
    readsPrec _ = readP_to_S geo'

antipode :: (Position a) => a -> a
antipode p = fromNVector (scale (toNVector p) (-1.0))

arc :: (Position a) => a -> a -> Arc
arc p1 p2 = Arc (toNVector p1) (toNVector p2)

destination :: (Position a) => a -> Degrees -> Meters -> a
destination p b d = fromNVector (add (scale v (cos ta)) (scale de (sin ta)))
  where
    v = toNVector p
    ed = normalise (cross north v) -- east direction vector at v
    nd = cross v ed -- north direction vector at v
    a = toRadians b -- azimuth in radians
    ta = meters d / meanEarthRadius -- angle travelled in radians
    de = add (scale nd (cos a)) (scale ed (sin a)) -- unit vector in the direction of the azimuth

distance :: (Position a) => a -> a -> Meters
distance p1 p2 = Meters (meanEarthRadius * atan2 (norm (cross v1 v2)) (dot v1 v2))
  where
    v1 = toNVector p1
    v2 = toNVector p2

geo :: Double -> Double -> GeoPos
geo lat lon = GeoPos (Degrees lat) (Degrees lon)

interpolate :: (Position a) => a -> Millis -> a -> Millis -> Millis -> a
interpolate p0 t0 p1 t1 ti = fromNVector (normalise (add v0 (scale (subtract v1 v0) s)))
  where
    v0 = toNVector p0
    v1 = toNVector p1
    s = fromIntegral (millis ti - millis t0) / fromIntegral (millis t1 - millis t0)

meanEarthRadius :: Double
meanEarthRadius = 6371008.8

midpoint :: (Position a) => [a] -> a
midpoint ps = fromNVector (normalise (foldl add zero vs))
  where
    vs = map toNVector ps

north :: (Position a) => a
north = fromNVector (NVector 0.0 0.0 1.0)

nvector :: Double -> Double -> Double -> NVector
nvector x' y' z' = normalise (NVector x' y' z')

south :: (Position a) => a
south = fromNVector (NVector 0.0 0.0 (-1.0))

add :: NVector -> NVector -> NVector
add a b = NVector x' y' z'
  where
    x' = x a + x b
    y' = y a + y b
    z' = z a + z b

subtract :: NVector -> NVector -> NVector
subtract a b = NVector x' y' z'
  where
    x' = x a - x b
    y' = y a - y b
    z' = z a - z b

cross :: NVector -> NVector -> NVector
cross a b = NVector x' y' z'
  where
    x' = y a * z b - z a * y b
    y' = z a * x b - x a * z b
    z' = x a * y b - y a * x b

dot :: NVector -> NVector -> Double
dot a b = x a * x b + y a * y b + z a * z b

norm :: NVector -> Double
norm a = sqrt ((x a * x a) + (y a * y a) + (z a * z a))

normalise :: NVector -> NVector
normalise a = scale a s
  where
    s = 1.0 / norm a

scale :: NVector -> Double -> NVector
scale a s = NVector x' y' z'
  where
    x' = x a * s
    y' = y a * s
    z' = z a * s

toRadians :: Degrees -> Double
toRadians d = degrees d * pi / 180.0

toDegrees :: Double -> Degrees
toDegrees r = Degrees (r / pi * 180.0)

zero :: NVector
zero = NVector 0.0 0.0 0.0

-- parsing

geo' :: ReadP GeoPos
geo' = do
    lat <- latitude'
    skipSpaces
    lon <- longitude'
    return (GeoPos (Degrees lat) (Degrees lon))

latitude' :: ReadP Double
latitude' = do
    d <- deg 2
    ms <- option 0 mOrMs
    h <- char 'N' <|> char 'S'
    return (decimal d ms (h == 'N'))

longitude' :: ReadP Double
longitude' = do
    d <- deg 3
    ms <- option 0 mOrMs
    h <- char 'E' <|> char 'W'
    return (decimal d ms (h == 'E'))

deg :: Int -> ReadP Double
deg n = do
    d <- numbers n
    return (fromIntegral d)

mOrMs :: ReadP Double
mOrMs = choice [minSec, minute]

minSec :: ReadP Double
minSec = do
    m <- numbers 2
    s <- numbers 2
    return (fromIntegral m / 60.0 + fromIntegral s / 3600.0)

minute :: ReadP Double
minute = do
    m <- numbers 2
    return (fromIntegral m / 60.0)

digit :: ReadP Char
digit = satisfy isDigit

numbers :: Int -> ReadP Int
numbers n = fmap read (count n digit)

decimal :: Double -> Double -> Bool -> Double
decimal d ms positive
    | positive = d + ms
    | otherwise = -(d + ms)
