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
    , ecefPosMetres
    ) where

import Data.Geo.Jord.Length
import Data.Geo.Jord.Vector3d

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

-- | instance of 'Vector3d'.
instance Vector3d EcefPosition where
    vecx v = toMetres (ex v)
    vecy v = toMetres (ey v)
    vecz v = toMetres (ez v)
    vector3d = ecefPosMetres

-- | 'EcefPosition' from given x, y and z length in _metres_.
--
-- @ex-ey@ plane is the equatorial plane, @ey@ is on the prime meridian, and @ez@ on the polar axis.
ecefPosMetres :: Double -> Double -> Double -> EcefPosition
ecefPosMetres x y z = EcefPosition (metres x) (metres y) (metres z)
