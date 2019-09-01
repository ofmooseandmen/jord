-- |
-- Module:      Data.Geo.Jord
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geographic position calculations (distance, bearing, intersection, etc...) on great circles using
-- the algorithms described in <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>.
-- and in <https://calhoun.nps.edu/bitstream/handle/10945/29516/sometacticalalgo00shud.pdf Shudde, Rex H. (1986). Some tactical algorithms for spherical geometry>
--
-- See <http://www.navlab.net/nvector Position calculations - simple and exact solutions>
--
-- See <http://www.movable-type.co.uk/scripts/latlong-vectors.html Vector-based geodesy>
--
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
module Data.Geo.Jord
    ( module Data.Geo.Jord.Angle
    , module Data.Geo.Jord.Bodies
    , module Data.Geo.Jord.Duration
    , module Data.Geo.Jord.Frames
    , module Data.Geo.Jord.Geodesic
    , module Data.Geo.Jord.GreatCircle
    , module Data.Geo.Jord.Kinematics
    , module Data.Geo.Jord.LatLong
    , module Data.Geo.Jord.Length
    , module Data.Geo.Jord.Position
    , module Data.Geo.Jord.Quantity
    , module Data.Geo.Jord.Rotation
    , module Data.Geo.Jord.Speed
    , module Data.Geo.Jord.Vector3d
    , jordVersion
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Bodies
import Data.Geo.Jord.Duration
import Data.Geo.Jord.Frames
import Data.Geo.Jord.Geodesic
import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.Kinematics
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.Position hiding (evec, nvec, nvh, nvNorthPole, nvSouthPole)
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Rotation
import Data.Geo.Jord.Speed
import Data.Geo.Jord.Vector3d

-- | version.
jordVersion :: String
jordVersion = "0.7.0.0"