-- |
-- Module:      Data.Geo.Jord.Earth
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Ellipsoidal and derived spherical earth models.
--
module Data.Geo.Jord.Earth
    ( Earth(..)
    , Ellipsoid(..)
    , eccentricity
    , meanRadius
    , polarRadius
    , spherical
    -- * Reference ellipsoids.
    , wgs84
    , grs80
    , wgs72
    -- * Spherical models dervived from reference ellipsoids.
    , s84
    , s80
    , s72
    , r84
    , r80
    , r72
    ) where

import Data.Geo.Jord.Length

-- | Earth model: ellipsoidal or spherical.
data Earth
    = Ellipsoidal Ellipsoid
    | Spherical Length
    deriving (Eq, Show)

-- | Primary ellipsoid parameters.
data Ellipsoid = Ellipsoid
    { equatorialRadius :: Length -- ^ equatorial radius or semi-major axis (a).
    , inverseFlattening :: Double -- ^ inverse flattening.
    } deriving (Eq, Show)

-- | Computes the eccentricity of the given 'Earth' model.
eccentricity :: Earth -> Double
eccentricity (Ellipsoidal e) = sqrt (1.0 - (b * b) / (a * a))
  where
    a = semiMajorAxis e
    b = semiMinorAxis a (flattening e)
eccentricity (Spherical _) = 0

-- | Computes the mean radius of the given 'Earth' model.
--
-- This radius can be used for geodetic calculations assuming a spherical earth model.
meanRadius :: Earth -> Length
meanRadius (Ellipsoidal e) = metres ((2.0 * a + b) / 3.0)
  where
    a = semiMajorAxis e
    b = semiMinorAxis a (flattening e)
meanRadius (Spherical r) = r

-- | Computes the polar radius or semi-minor axis (b) of the given 'Earth' model.
polarRadius :: Earth -> Length
polarRadius (Ellipsoidal e) = metres (semiMinorAxis a f)
  where
    a = semiMajorAxis e
    f = flattening e
polarRadius (Spherical r) = r

-- | Spherical model derived from given model.
spherical :: Earth -> Earth
spherical e = Spherical (meanRadius e)

-- | World Geodetic System WGS84 ellipsoid.
wgs84 :: Earth
wgs84 = Ellipsoidal (Ellipsoid (metres 6378137.0) 298.257223563)

-- | Geodetic Reference System 1980 ellipsoid.
grs80 :: Earth
grs80 = Ellipsoidal (Ellipsoid (metres 6378137.0) 298.257222101)

-- | World Geodetic System WGS72 ellipsoid.
wgs72 :: Earth
wgs72 = Ellipsoidal (Ellipsoid (metres 6378135.0) 298.26)

-- | Spherical earth model derived from 'wgs84'.
s84 :: Earth
s84 = spherical wgs84

-- | Spherical earth model derived from 'grs80'.
s80 :: Earth
s80 = spherical grs80

-- | Spherical earth model derived from 'wgs72'.
s72 :: Earth
s72 = spherical wgs72

-- | Mean earth radius derived from the 'wgs84' ellipsoid.
r84 :: Length
r84 = meanRadius s84

-- | Mean earth radius derived from the 'grs80' ellipsoid.
r80 :: Length
r80 = meanRadius s80

-- | Mean earth radius derived from the 'wgs72' ellipsoid.
r72 :: Length
r72 = meanRadius s72

-- | semi-major axis (a) in metres.
semiMajorAxis :: Ellipsoid -> Double
semiMajorAxis = toMetres . equatorialRadius

-- | Computes the polar semi-minor axis (b) from semi major axis @a@ and flattening @f@.
semiMinorAxis :: Double -> Double -> Double
semiMinorAxis a f = a * (1.0 - f)

-- | flattening of ellispoid @e@
flattening :: Ellipsoid -> Double
flattening e = 1.0 / inverseFlattening e
