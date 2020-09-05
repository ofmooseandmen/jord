-- |
-- Module:      Data.Geo.Jord.Ellipsoid
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with ellipsoids (including spheres).
--
-- see "Data.Geo.Jord.Ellipsoids" for supported ellipsoids.
module Data.Geo.Jord.Ellipsoid
    ( Ellipsoid
    , equatorialRadius
    , polarRadius
    , eccentricity
    , flattening
    , ellispoid
    , sphere
    , toSphere
    , isSphere
    , meanRadius
    ) where

import Data.Geo.Jord.Length

-- | Parameters of an ellispoid describing the surface of a celestial body.
--  An ellispoid is a circle if  its 'equatorialRadius' and 'polarRadius' are
-- equal (both its 'eccentricity' and 'flattening' are 0); it is used to represent
-- a celestial body as a sphere.
data Ellipsoid =
    Ellipsoid
        { equatorialRadius :: !Length -- ^ equatorial radius or semi-major axis (a).
        , polarRadius :: !Length -- ^ polar radius or semi-minor axis (b).
        , eccentricity :: !Double -- ^ eccentricity
        , flattening :: !Double -- ^ flattening
        }
    deriving (Eq, Show)

-- | @ellispoid eqr invf@: ellipsoid with equatorial radius @eqr@ and inverse flattening @invf@.
ellispoid :: Length -> Double -> Ellipsoid
ellispoid eqr invf = Ellipsoid eqr (metres b) e f
  where
    a = toMetres eqr
    f = 1.0 / invf
    b = a * (1.0 - f)
    e = sqrt (1.0 - (b * b) / (a * a))

-- | @sphere r@: ellipsoid with equatorial & polar radius radius @r@.
-- The returned ellipsoid is a sphere.
sphere :: Length -> Ellipsoid
sphere r = Ellipsoid r r 0.0 0.0

-- | @toSphere e@: sphere from mean radius of ellipsoid @e@.
toSphere :: Ellipsoid -> Ellipsoid
toSphere = sphere . meanRadius

-- | @isSphere e@ returns True if ellipsoid @e@ is a sphere.
isSphere :: Ellipsoid -> Bool
isSphere e = eccentricity e == 0.0

-- | @meanRadius e@ computes the mean radius of ellipsoid @e@.
meanRadius :: Ellipsoid -> Length
meanRadius e = metres ((2.0 * a + b) / 3.0)
  where
    a = toMetres . equatorialRadius $ e
    b = toMetres . polarRadius $ e