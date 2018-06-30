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
    ( module Data.Geo.Jord.GeoPos
    , module Data.Geo.Jord.GreatCircle
    , module Data.Geo.Jord.NVector
    ) where

import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.NVector
