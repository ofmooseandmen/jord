-- |
-- Module:      Data.Geo.Jord.Ellipsoid
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with ellipsoids (including spheres).
--
-- see 'Ellipsoids' for supported ellipsoids.
--
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

ellispoid :: Length -> Double -> Ellipsoid
ellispoid eqr invf = Ellipsoid eqr (metres b) e f
  where
    a = toMetres eqr
    f = 1.0 / invf
    b = a * (1.0 - f)
    e = sqrt (1.0 - (b * b) / (a * a))

sphere :: Length -> Ellipsoid
sphere r = Ellipsoid r r 0.0 0.0

toSphere :: Ellipsoid -> Ellipsoid
toSphere = sphere . meanRadius

isSphere :: Ellipsoid -> Bool
isSphere e = eccentricity e == 0.0

meanRadius :: Ellipsoid -> Length
meanRadius e = metres ((2.0 * a + b) / 3.0)
  where
    a = toMetres . equatorialRadius $ e
    b = toMetres . polarRadius $ e
