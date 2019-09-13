-- | 
-- Module:      Data.Geo.Jord.Transformations 
-- Copyright:   (c) 2019 Cedric Liegeois 
-- License:     BSD3 
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> 
-- Stability:   experimental 
-- Portability: portable 
--
-- Coordinates transformations.
--
-- params: tx ty tz s rx ry rz
-- rates:  tx ty tz s rx ry rz
--
-- translations in millimetres, rates in millimetres per year
-- scale factors in parts per billion, rates in parts per billion per year
-- rotations in milliarcseconds, rates in milliarcseconds per year
--
-- This module has been generated.
--
module Data.Geo.Jord.Transformations where

import Data.Geo.Jord.Model
import Data.Geo.Jord.Transformation

-- WGS84 to NAD83 transformation parameters.
from_WGS84_to_NAD83 :: StaticTx
from_WGS84_to_NAD83 =
    staticTx
        (ModelId "WGS84")
        (ModelId "WGS84")
        (txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599))

-- ITRF2014 to ITRF2008 transformation parameters.
from_ITRF2014_to_ITRF2008 :: DynamicTx
from_ITRF2014_to_ITRF2008 =
    dynamicTx
        (ModelId "ITRF2014")
        (ModelId "ITRF2008")
        (TxParams15
             (Epoch 2010.0)
             (txParams7 (1.6, 1.9, 2.4) (-0.2) (0.0, 0.0, 0.0))
             (txRates (0.0, 0.0, -0.1) 0.3 (0.0, 0.0, 0.0)))

-- ITRF2014 to ETRF2000 transformation parameters.
from_ITRF2014_to_ETRF2000 :: DynamicTx
from_ITRF2014_to_ETRF2000 =
    dynamicTx
        (ModelId "ITRF2014")
        (ModelId "ETRF2000")
        (TxParams15
             (Epoch 2000.0)
             (txParams7 (53.7, 51.2, -55.1) 1.2 (0.891, 5.39, -8.712))
             (txRates (0.1, 0.1, -1.9) 0.11 (0.81, 0.49, -0.792)))

