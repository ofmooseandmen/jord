-- | 
-- Module:      Data.Geo.Jord.Ellipsoids 
-- Copyright:   (c) 2019 Cedric Liegeois 
-- License:     BSD3 
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> 
-- Stability:   experimental 
-- Portability: portable 
--
-- Common ellipsoids of different celestial bodies.
--
-- This module has been generated.
--
module Data.Geo.Jord.Ellipsoids where

import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Length

-- World Geodetic 84 Ellipsoid.
eWGS84 :: Ellipsoid
eWGS84 = ellispoid (metres 6378137.0) 298.257223563

-- Geodetic Reference System 1980 Ellipsoid.
eGRS80 :: Ellipsoid
eGRS80 = ellispoid (metres 6378137.0) 298.257222101

-- World Geodetic 72 Ellipsoid.
eWGS72 :: Ellipsoid
eWGS72 = ellispoid (metres 6378135.0) 298.26

-- Mars Orbiter Laser Altimeter Ellipsoid.
eMars2000 :: Ellipsoid
eMars2000 = ellispoid (metres 3398627.0) 169.8

-- Moon IAU/IAG Sphere.
eMoon :: Ellipsoid
eMoon = sphere (metres 1737400.0)

