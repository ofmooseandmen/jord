-- | 
-- Module:      Data.Geo.Jord.Models 
-- Copyright:   (c) 2019 Cedric Liegeois 
-- License:     BSD3 
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> 
-- Stability:   experimental 
-- Portability: portable 
--
-- Common ellipsoidal and spherical models. 
--
-- This module has been generated.
--
module Data.Geo.Jord.Models where

import Data.Geo.Jord.Ellipsoids
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Model

-- | World Geodetic System 1984.
data WGS84 = 
    WGS84

instance Model WGS84 where
    modelId _ = ModelId "WGS84"
    surface _ = eWGS84
    longitudeRange _ = L180

instance Eq WGS84 where
    _ == _ = True

instance Show WGS84 where
    show m = show (modelId m)

instance Ellipsoidal WGS84

-- | Geodetic Reference System 1980.
data GRS80 = 
    GRS80

instance Model GRS80 where
    modelId _ = ModelId "GRS80"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq GRS80 where
    _ == _ = True

instance Show GRS80 where
    show m = show (modelId m)

instance Ellipsoidal GRS80

-- | World Geodetic System 1972.
data WGS72 = 
    WGS72

instance Model WGS72 where
    modelId _ = ModelId "WGS72"
    surface _ = eWGS72
    longitudeRange _ = L180

instance Eq WGS72 where
    _ == _ = True

instance Show WGS72 where
    show m = show (modelId m)

instance Ellipsoidal WGS72

-- | European Terrestrial Reference System 1989.
data ETRS89 = 
    ETRS89

instance Model ETRS89 where
    modelId _ = ModelId "ETRS89"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ETRS89 where
    _ == _ = True

instance Show ETRS89 where
    show m = show (modelId m)

instance Ellipsoidal ETRS89

-- | North American Datum of 1983.
data NAD83 = 
    NAD83

instance Model NAD83 where
    modelId _ = ModelId "NAD83"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq NAD83 where
    _ == _ = True

instance Show NAD83 where
    show m = show (modelId m)

instance Ellipsoidal NAD83

-- | Mars Orbiter Laser Altimeter.
data Mars2000 = 
    Mars2000

instance Model Mars2000 where
    modelId _ = ModelId "Mars2000"
    surface _ = eMars2000
    longitudeRange _ = L360

instance Eq Mars2000 where
    _ == _ = True

instance Show Mars2000 where
    show m = show (modelId m)

instance Ellipsoidal Mars2000

-- | International Terrestrial Reference System (2014).
data ITRF2014 = 
    ITRF2014

instance Model ITRF2014 where
    modelId _ = ModelId "ITRF2014"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ITRF2014 where
    _ == _ = True

instance Show ITRF2014 where
    show m = show (modelId m)

instance Ellipsoidal ITRF2014

instance EllipsoidalT0 ITRF2014 where
    epoch _ = Epoch 2010 0

-- | International Terrestrial Reference System (2008).
data ITRF2008 = 
    ITRF2008

instance Model ITRF2008 where
    modelId _ = ModelId "ITRF2008"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ITRF2008 where
    _ == _ = True

instance Show ITRF2008 where
    show m = show (modelId m)

instance Ellipsoidal ITRF2008

instance EllipsoidalT0 ITRF2008 where
    epoch _ = Epoch 2005 0

-- | International Terrestrial Reference System (2005).
data ITRF2005 = 
    ITRF2005

instance Model ITRF2005 where
    modelId _ = ModelId "ITRF2005"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ITRF2005 where
    _ == _ = True

instance Show ITRF2005 where
    show m = show (modelId m)

instance Ellipsoidal ITRF2005

instance EllipsoidalT0 ITRF2005 where
    epoch _ = Epoch 2000 0

-- | International Terrestrial Reference System (2000).
data ITRF2000 = 
    ITRF2000

instance Model ITRF2000 where
    modelId _ = ModelId "ITRF2000"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ITRF2000 where
    _ == _ = True

instance Show ITRF2000 where
    show m = show (modelId m)

instance Ellipsoidal ITRF2000

instance EllipsoidalT0 ITRF2000 where
    epoch _ = Epoch 1997 0

-- | International Terrestrial Reference System (93).
data ITRF93 = 
    ITRF93

instance Model ITRF93 where
    modelId _ = ModelId "ITRF93"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ITRF93 where
    _ == _ = True

instance Show ITRF93 where
    show m = show (modelId m)

instance Ellipsoidal ITRF93

instance EllipsoidalT0 ITRF93 where
    epoch _ = Epoch 1988 0

-- | International Terrestrial Reference System (91).
data ITRF91 = 
    ITRF91

instance Model ITRF91 where
    modelId _ = ModelId "ITRF91"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ITRF91 where
    _ == _ = True

instance Show ITRF91 where
    show m = show (modelId m)

instance Ellipsoidal ITRF91

instance EllipsoidalT0 ITRF91 where
    epoch _ = Epoch 1988 0

-- | World Geodetic System 1984 (G1762).
data WGS84_G1762 = 
    WGS84_G1762

instance Model WGS84_G1762 where
    modelId _ = ModelId "WGS84_G1762"
    surface _ = eWGS84
    longitudeRange _ = L180

instance Eq WGS84_G1762 where
    _ == _ = True

instance Show WGS84_G1762 where
    show m = show (modelId m)

instance Ellipsoidal WGS84_G1762

instance EllipsoidalT0 WGS84_G1762 where
    epoch _ = Epoch 2005 0

-- | World Geodetic System 1984 (G1674).
data WGS84_G1674 = 
    WGS84_G1674

instance Model WGS84_G1674 where
    modelId _ = ModelId "WGS84_G1674"
    surface _ = eWGS84
    longitudeRange _ = L180

instance Eq WGS84_G1674 where
    _ == _ = True

instance Show WGS84_G1674 where
    show m = show (modelId m)

instance Ellipsoidal WGS84_G1674

instance EllipsoidalT0 WGS84_G1674 where
    epoch _ = Epoch 2005 0

-- | World Geodetic System 1984 (G1150).
data WGS84_G1150 = 
    WGS84_G1150

instance Model WGS84_G1150 where
    modelId _ = ModelId "WGS84_G1150"
    surface _ = eWGS84
    longitudeRange _ = L180

instance Eq WGS84_G1150 where
    _ == _ = True

instance Show WGS84_G1150 where
    show m = show (modelId m)

instance Ellipsoidal WGS84_G1150

instance EllipsoidalT0 WGS84_G1150 where
    epoch _ = Epoch 2001 0

-- | European Terrestrial Reference System (2000).
data ETRF2000 = 
    ETRF2000

instance Model ETRF2000 where
    modelId _ = ModelId "ETRF2000"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq ETRF2000 where
    _ == _ = True

instance Show ETRF2000 where
    show m = show (modelId m)

instance Ellipsoidal ETRF2000

instance EllipsoidalT0 ETRF2000 where
    epoch _ = Epoch 2005 0

-- | NAD83 (Continuously Operating Reference Station 1996).
data NAD83_CORS96 = 
    NAD83_CORS96

instance Model NAD83_CORS96 where
    modelId _ = ModelId "NAD83_CORS96"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq NAD83_CORS96 where
    _ == _ = True

instance Show NAD83_CORS96 where
    show m = show (modelId m)

instance Ellipsoidal NAD83_CORS96

instance EllipsoidalT0 NAD83_CORS96 where
    epoch _ = Epoch 1997 0

-- | Geocentric Datum Of Australia 1994.
data GDA94 = 
    GDA94

instance Model GDA94 where
    modelId _ = ModelId "GDA94"
    surface _ = eGRS80
    longitudeRange _ = L180

instance Eq GDA94 where
    _ == _ = True

instance Show GDA94 where
    show m = show (modelId m)

instance Ellipsoidal GDA94

instance EllipsoidalT0 GDA94 where
    epoch _ = Epoch 1994 0

-- | Spherical Earth model derived from WGS84 ellipsoid.
data S84 = 
    S84

instance Model S84 where
    modelId _ = ModelId "S84"
    surface _ = toSphere eWGS84
    longitudeRange _ = L180

instance Eq S84 where
    _ == _ = True

instance Show S84 where
    show m = show (modelId m)

instance Spherical S84

-- | Spherical Mars model derived from Mars2000 ellipsoid.
data SMars2000 = 
    SMars2000

instance Model SMars2000 where
    modelId _ = ModelId "SMars2000"
    surface _ = toSphere eMars2000
    longitudeRange _ = L360

instance Eq SMars2000 where
    _ == _ = True

instance Show SMars2000 where
    show m = show (modelId m)

instance Spherical SMars2000

-- | Moon IAU/IAG.
data Moon = 
    Moon

instance Model Moon where
    modelId _ = ModelId "Moon"
    surface _ = toSphere eMoon
    longitudeRange _ = L180

instance Eq Moon where
    _ == _ = True

instance Show Moon where
    show m = show (modelId m)

instance Spherical Moon

