-- |
-- Module:      Data.Geo.Jord.Ellipsoid
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for defining and working with reference ellipsoids.
--
module Data.Geo.Jord.Ellipsoid
    ( Ellipsoid(..)
    , eccentricity
    , meanRadius
    , polarRadius
    , wgs84
    ) where

import Data.Geo.Jord.Length

-- | Primary ellipsoid parameters.
data Ellipsoid = Ellipsoid
    { equatorialRadius :: Length -- ^ equatorial radius or semi-major axis (a).
    , inverseFlattening :: Double -- ^ inverse flattening.
    } deriving (Eq, Show)

-- | Computes the eccentricity of the given 'Ellipsoid'.
eccentricity :: Ellipsoid -> Double
eccentricity e = sqrt (1.0 - (b * b) / (a * a))
  where
    a = semiMajorAxis e
    b = semiMinorAxis a (inverseFlattening e)

-- | Computes the mean radius of the given 'Ellipsoid'.
--
-- This radius can be used for geodetic calculations assuming a spherical earth model.
meanRadius :: Ellipsoid -> Length
meanRadius e = metres ((2.0 * a + b) / 3.0)
  where
    a = semiMajorAxis e
    b = semiMinorAxis a (inverseFlattening e)

-- | Computes the polar radius or semi-minor axis (b) of the given 'Ellipsoid'.
polarRadius :: Ellipsoid -> Length
polarRadius e = metres (semiMinorAxis a f)
  where
    a = semiMajorAxis e
    f = inverseFlattening e

-- | World Geodetic System WGS84 ellipsoid.
wgs84 :: Ellipsoid
wgs84 = Ellipsoid (metres 6378137.0) (1.0 / 298.257223563)

-- | semi-major axis (a) in metres.
semiMajorAxis :: Ellipsoid -> Double
semiMajorAxis = toMetres . equatorialRadius

-- | Computes the polar semi-minor axis (b) from @a@ anf @f@.
semiMinorAxis :: Double -> Double -> Double
semiMinorAxis a f = a * (1.0 - f)
