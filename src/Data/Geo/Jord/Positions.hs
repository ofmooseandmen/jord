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
    , elevatedLatLong
    , elevatedNVector
    , ecefPosition
    , ecefPositionMetres
    , spherical
    , ellipsoidal
    , northPole
    , southPole
    ) where

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

spherical :: a -> Length -> GeoPos a Length
spherical = GeoPos

ellipsoidal :: a -> Ellipsoid -> GeoPos a Ellipsoid
ellipsoidal = GeoPos

-- | Horizontal position of the North Pole.
northPole :: NVector
northPole = nvector 0.0 0.0 1.0

-- | Horizontal position of the South Pole.
southPole :: NVector
southPole = nvector 0.0 0.0 (-1.0)
