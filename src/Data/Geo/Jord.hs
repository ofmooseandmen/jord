-- |
-- Module:      Data.Geo.Jord
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geographic position calculations (distance, bearing, intersection, etc...) on great circles using
-- the algorithms described in <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>.
--
-- See <http://www.navlab.net/nvector Position calculations - simple and exact solutions>
--
-- See <http://www.movable-type.co.uk/scripts/latlong-vectors.html Vector-based geodesy>
--
module Data.Geo.Jord
    ( module Data.Geo.Jord.Angle
    , module Data.Geo.Jord.Ellipsoid
    , module Data.Geo.Jord.Eval
    , module Data.Geo.Jord.LatLong
    , module Data.Geo.Jord.Length
    , module Data.Geo.Jord.NVector
    , module Data.Geo.Jord.Position
    , module Data.Geo.Jord.Quantity
    , module Data.Geo.Jord.Spherical.Geodetics
    , module Data.Geo.Jord.Spherical.GreatCircle
    , jordVersion
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Eval
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Position
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Spherical.Geodetics
import Data.Geo.Jord.Spherical.GreatCircle

-- | version.
jordVersion :: String
jordVersion = "0.3.0.0"
