{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:      Data.Geo.Jord.Positions
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions TODO.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Positions
    ( AngularPosition(..)
    , NVectorPosition(..)
    , EcefPosition(..)
    , GeoPos(..)
    , VTransform(..)
    , CTransform(..)
    , elevatedLatLong
    , elevatedNVector
    , ecefPosition
    , ecefPositionMetres
    , sphericalPos
    , ellipsoidalPos
    , fromNVector
    , fromLatLong
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector

-- | An earth position defined by its latitude, longitude and height.
data AngularPosition = AngularPosition
    { getLatLong :: LatLong
    , apHeight :: Double
    } deriving (Eq, Show)

-- | An earth position defined by its n-vector and height.
data NVectorPosition = NVectorPosition
    { getNVector :: NVector
    , nvpHeight :: Double
    } deriving (Eq, Show)

-- | An earth position expressed in the Earth Centered, Earth Fixed (ECEF) coordinates system.
--
-- @ex-ey@ plane is the equatorial plane, @ey@ is on the prime meridian, and @ez@ on the polar axis.
data EcefPosition = EcefPosition
    { ex :: Length
    , ey :: Length
    , ez :: Length
    } deriving (Eq, Show)

elevatedLatLong :: LatLong -> Double -> AngularPosition
elevatedLatLong = AngularPosition

elevatedNVector :: NVector -> Double -> NVectorPosition
elevatedNVector = NVectorPosition

ecefPosition :: Length -> Length -> Length -> EcefPosition
ecefPosition = EcefPosition

ecefPositionMetres :: Double -> Double -> Double -> EcefPosition
ecefPositionMetres x y z = EcefPosition (metres x) (metres y) (metres z)

data GeoPos a b = GeoPos
    { pos :: a
    , model :: b
    } deriving (Eq, Show)

sphericalPos :: a -> Length -> GeoPos a Length
sphericalPos = GeoPos

ellipsoidalPos :: a -> Ellipsoid -> GeoPos a Ellipsoid
ellipsoidalPos = GeoPos

class VTransform a where
    vec :: GeoPos a b -> GeoPos NVector b
    unvec :: Double -> GeoPos NVector b -> GeoPos a b

instance VTransform LatLong where
    vec (GeoPos ll m) = GeoPos (fromLatLong ll) m
    unvec _ (GeoPos nv m) = GeoPos (fromNVector nv) m

instance VTransform NVectorPosition where
    vec (GeoPos (NVectorPosition nv _) m) = GeoPos nv m
    unvec h (GeoPos nv m) = GeoPos (NVectorPosition nv h) m

instance VTransform AngularPosition where
    vec (GeoPos (AngularPosition ll _) m) = GeoPos (fromLatLong ll) m
    unvec h (GeoPos nv m) = GeoPos (AngularPosition (fromNVector nv) h) m

class CTransform a b where
    toEcef :: GeoPos a b -> GeoPos EcefPosition b
    fromEcef :: GeoPos EcefPosition b -> GeoPos a b

fromNVector :: NVector -> LatLong
fromNVector v = latLong lat lon
  where
    lat = atan2' (nz v) (sqrt (nx v * nx v + ny v * ny v))
    lon = atan2' (ny v) (nx v)

fromLatLong :: LatLong -> NVector
fromLatLong g = nvector x' y' z'
  where
    lat = latitude g
    lon = longitude g
    cl = cos' lat
    x' = cl * cos' lon
    y' = cl * sin' lon
    z' = sin' lat

-- | Horizontal position of the North Pole.
northPole :: NVector
northPole = nvector 0.0 0.0 1.0

-- | Horizontal position of the South Pole.
southPole :: NVector
southPole = nvector 0.0 0.0 (-1.0)
