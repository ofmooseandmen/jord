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
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
-- TODO: doc
--
module Data.Geo.Jord.Positions
    ( AngularPosition(..)
    , NVectorPosition(..)
    , EcefPosition(..)
    , angularPos
    , nvectorPos
    , ecefPos
    , ecefPosMetres
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity (Norm(..))

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
--
-- Note: on a spherical model earth, an n-vector is equivalent to a normalised version of an (ECEF) cartesian coordinate.
data EcefPosition = EcefPosition
    { ex :: Length
    , ey :: Length
    , ez :: Length
    } deriving (Eq, Show)

-- | 'NedVector' norm.
instance Norm EcefPosition Length where
    norm a = metres (sqrt (x * x + y * y + z * z))
      where
        x = toMetres (ex a)
        y = toMetres (ey a)
        z = toMetres (ez a)

angularPos :: LatLong -> Double -> AngularPosition
angularPos = AngularPosition

nvectorPos :: NVector -> Double -> NVectorPosition
nvectorPos = NVectorPosition

ecefPos :: Length -> Length -> Length -> EcefPosition
ecefPos = EcefPosition

ecefPosMetres :: Double -> Double -> Double -> EcefPosition
ecefPosMetres x y z = EcefPosition (metres x) (metres y) (metres z)

-- | Horizontal position of the North Pole.
northPole :: NVector
northPole = NVector 0.0 0.0 1.0

-- | Horizontal position of the South Pole.
southPole :: NVector
southPole = NVector 0.0 0.0 (-1.0)
