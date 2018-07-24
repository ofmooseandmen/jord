{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:      Data.Geo.Jord.EcefPosition
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Earth Centered, Earth Fixed (ECEF) position.
--
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
module Data.Geo.Jord.EcefPosition
    ( EcefPosition(..)
    , ecefPos
    , ecefPosMetres
    ) where

import Data.Geo.Jord.Length
import Data.Geo.Jord.Quantity (Norm(..))

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

ecefPos :: Length -> Length -> Length -> EcefPosition
ecefPos = EcefPosition

ecefPosMetres :: Double -> Double -> Double -> EcefPosition
ecefPosMetres x y z = EcefPosition (metres x) (metres y) (metres z)
