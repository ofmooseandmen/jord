-- |
-- Module:      Data.Geo.Jord.EcefPosition
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Earth Centred, Earth Fixed (ECEF) position.
--
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
module Data.Geo.Jord.EcefPosition
    ( EcefPosition
    , ecef
    , ecefMetres
    , ex
    , ey
    , ez
    ) where

import Data.Geo.Jord.Length
import Data.Geo.Jord.Vector3d

-- | An earth position expressed in the Earth Centred, Earth Fixed (ECEF) coordinates system.
--
-- @ex-ey@ plane is the equatorial plane, @ey@ is on the prime meridian, and @ez@ on the polar axis.
--
-- Note: on a spherical model earth, an n-vector is equivalent to a normalised version of an (ECEF) cartesian coordinate.
newtype EcefPosition =
    EcefPosition Vector3d
    deriving (Eq, Show)

instance IsVector3d EcefPosition where
    vec (EcefPosition v) = v

-- | 'EcefPosition' from given x, y and z length.
--
-- @ex-ey@ plane is the equatorial plane, @ey@ is on the prime meridian, and @ez@ on the polar axis.
ecef :: Length -> Length -> Length -> EcefPosition
ecef x y z = EcefPosition (Vector3d (toMetres x) (toMetres y) (toMetres z))

-- | 'EcefPosition' from given x, y and z length in _metres_.
--
-- @ex-ey@ plane is the equatorial plane, @ey@ is on the prime meridian, and @ez@ on the polar axis.
ecefMetres :: Double -> Double -> Double -> EcefPosition
ecefMetres x y z = ecef (metres x) (metres y) (metres z)

-- | x coordinate of the given '$tc'EcefPosition'.
ex :: EcefPosition -> Length
ex (EcefPosition v) = metres (vx v)

-- | y coordinate of the given '$tc'EcefPosition'.
ey :: EcefPosition -> Length
ey (EcefPosition v) = metres (vy v)

-- | z coordinate of the given '$tc'EcefPosition'.
ez :: EcefPosition -> Length
ez (EcefPosition v) = metres (vz v)
