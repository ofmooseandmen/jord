-- | 
-- Module:      Data.Geo.Jord.Ellipsoids 
-- Copyright:   (c) 2020 Cedric Liegeois 
-- License:     BSD3 
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> 
-- Stability:   experimental 
-- Portability: portable 
--
-- Common ellipsoids of different celestial bodies.
--
-- This module has been generated.
module Data.Geo.Jord.Ellipsoids where

import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Length

-- | World Geodetic 84 Ellipsoid.
eWGS84 :: Ellipsoid
eWGS84 = ellispoid (metres 6378137.0) 298.257223563

-- | Geodetic Reference System 1980 Ellipsoid.
eGRS80 :: Ellipsoid
eGRS80 = ellispoid (metres 6378137.0) 298.257222101

-- | World Geodetic 72 Ellipsoid.
eWGS72 :: Ellipsoid
eWGS72 = ellispoid (metres 6378135.0) 298.26

-- | IUGG 1924 Ellipsoid.
eIntl1924 :: Ellipsoid
eIntl1924 = ellispoid (metres 6378388.0) 297.0

-- | Original definition Ellipsoid (1796).
eAiry1830 :: Ellipsoid
eAiry1830 = ellispoid (metres 6377563.396) 299.3249646

-- | Not specified, use only in cases where geodetic datum is unknown.
eAiryModified :: Ellipsoid
eAiryModified = ellispoid (metres 6377340.189) 299.3249646

-- | Bessel 1841 Ellipsoid.
eBessel1841 :: Ellipsoid
eBessel1841 = ellispoid (metres 6377397.155) 299.1528128

-- | Clarke (1866) Ellipsoid.
eClarke1866 :: Ellipsoid
eClarke1866 = ellispoid (metres 6378206.4) 294.978698214

-- |  Clarke (1880) Ellipsoid.
eClarke1880IGN :: Ellipsoid
eClarke1880IGN = ellispoid (metres 6378249.2) 293.466021294

-- | Mars Orbiter Laser Altimeter Ellipsoid.
eMars2000 :: Ellipsoid
eMars2000 = ellispoid (metres 3398627.0) 169.8

-- | Moon IAU/IAG Sphere.
eMoon :: Ellipsoid
eMoon = sphere (metres 1737400.0)

