-- | 
-- Module:      Data.Geo.Jord.Txs 
-- Copyright:   (c) 2019 Cedric Liegeois 
-- License:     BSD3 
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> 
-- Stability:   experimental 
-- Portability: portable 
--
-- Coordinates transformation parameters between various models.
--
--    * params: tx ty tz s rx ry rz
--
--    * rates:  tx ty tz s rx ry rz
--
--    * translations in millimetres, rates in millimetres per year
--
--    * scale factors in parts per billion, rates in parts per billion per year
--
--    * rotations in milliarcseconds, rates in milliarcseconds per year
--
-- This module has been generated.
--
module Data.Geo.Jord.Txs where

import Data.Geo.Jord.Model
import Data.Geo.Jord.Tx

-- | WGS84 to NAD83 transformation parameters.
from_WGS84_to_NAD83 :: Tx TxParams7
from_WGS84_to_NAD83 =
    Tx (ModelId "WGS84")
        (ModelId "NAD83")
        (txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599))

-- | ITRF2014 to ITRF2008 transformation parameters.
from_ITRF2014_to_ITRF2008 :: Tx TxParams15
from_ITRF2014_to_ITRF2008 =
    Tx (ModelId "ITRF2014")
        (ModelId "ITRF2008")
        (TxParams15
             (Epoch 2010.0)
             (txParams7 (1.6, 1.9, 2.4) (-2.0e-2) (0.0, 0.0, 0.0))
             (txRates (0.0, 0.0, -0.1) 3.0e-2 (0.0, 0.0, 0.0)))

-- | ITRF2014 to ITRF2005 transformation parameters.
from_ITRF2014_to_ITRF2005 :: Tx TxParams15
from_ITRF2014_to_ITRF2005 =
    Tx (ModelId "ITRF2014")
        (ModelId "ITRF2005")
        (TxParams15
             (Epoch 2010.0)
             (txParams7 (2.6, 1.0, -2.3) 0.92 (0.0, 0.0, 0.0))
             (txRates (0.3, 0.0, -0.1) 3.0e-2 (0.0, 0.0, 0.0)))

-- | ITRF2014 to ITRF2000 transformation parameters.
from_ITRF2014_to_ITRF2000 :: Tx TxParams15
from_ITRF2014_to_ITRF2000 =
    Tx (ModelId "ITRF2014")
        (ModelId "ITRF2000")
        (TxParams15
             (Epoch 2010.0)
             (txParams7 (0.7, 1.2, -26.1) 2.12 (0.0, 0.0, 0.0))
             (txRates (0.1, 0.1, -1.9) 0.11 (0.0, 0.0, 0.0)))

-- | ITRF2014 to ETRF2000 transformation parameters.
from_ITRF2014_to_ETRF2000 :: Tx TxParams15
from_ITRF2014_to_ETRF2000 =
    Tx (ModelId "ITRF2014")
        (ModelId "ETRF2000")
        (TxParams15
             (Epoch 2000.0)
             (txParams7 (53.7, 51.2, -55.1) 1.02 (0.891, 5.39, -8.712))
             (txRates (0.1, 0.1, -1.9) 0.11 (8.1e-2, 0.49, -0.792)))

-- | ITRF2000 to NAD83 (CORS96) transformation parameters.
from_ITRF2000_to_NAD83_CORS96 :: Tx TxParams15
from_ITRF2000_to_NAD83_CORS96 =
    Tx (ModelId "ITRF2000")
        (ModelId "NAD83_CORS96")
        (TxParams15
             (Epoch 1997.0)
             (txParams7 (995.6, -1901.3, -521.5) 0.62 (25.915, 9.426, 11.599))
             (txRates (0.7, -0.7, 0.5) (-0.18) (6.7e-2, -0.757, -5.1e-2)))

-- | Graph of all static transformations.
staticTxs :: TxGraph TxParams7
staticTxs =
    txGraph
        [ from_WGS84_to_NAD83
        ]

-- | Graph of all dynamic transformations.
dynamicTxs :: TxGraph TxParams15
dynamicTxs =
    txGraph
        [ from_ITRF2014_to_ITRF2008
        , from_ITRF2014_to_ITRF2005
        , from_ITRF2014_to_ITRF2000
        , from_ITRF2014_to_ETRF2000
        , from_ITRF2000_to_NAD83_CORS96
        ]
