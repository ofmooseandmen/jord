-- |
-- Module:      Data.Geo.Jord
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geographic position calculations using either spherical or ellipsoidal models. Calculations require
-- that all positions are in the same model, thus preventing accidental mix of coordinate systems (e.g.
-- distance between a WGS84 position and an ITRF2014 position). Positions can be defined either in terms of
-- geodetic latitude, longitude and height or by their geocentric coordinates.
--
-- Different calculations are available depending on whether a sphere or an ellispoid is used to
-- represent the surface of the celestial body.
--
-- Spherical models are the simplest approximation and are satisfactory for many purposes. A large range
-- of vector-based calculations on great circles are available, including distance, destination position,
-- initial/final bearings, intersections. Solutions for some kinematics problems are also supported: closest
-- point of approach and intercept.
--
--     * "Data.Geo.Jord.GreatCircle"
--
--     * "Data.Geo.Jord.Kinematics"
--
-- Ellipsoidal models provide a better accuracy if needed. Solutions to the direct and inverse geodesic problems are
-- implemented using the Vicenty formulaes. Vector-based calculations in local reference frames are also provided.
-- Coordinate transformations between different ellispoidal models are possible for both static and dynamic reference
-- frames.
--
--    * "Data.Geo.Jord.Geodesic"
--
--    * "Data.Geo.Jord.LocalFrames"
--
--    * "Data.Geo.Jord.Transformation"
--
-- Acknowledgments and extra documentation:
--
--     * All vector-based calculations are based on the paper <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>.
--       More documentation is available at <http://www.navlab.net/nvector Position calculations - simple and exact solutions>.
--
--     * The Kinematics module implements the algorithms described in <https://calhoun.nps.edu/bitstream/handle/10945/29516/sometacticalalgo00shud.pdf Shudde, Rex H. (1986). Some tactical algorithms for spherical geometry>
--
--     * The Geodesic module implements the algorithms described in <http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf T Vincenty, "Direct and Inverse Solutions of Geodesics on the Ellipsoid with application of nested equations", Survey Review, vol XXIII no 176, 1975.>
--
--     * <https://github.com/chrisveness/geodesy Libraries of geodesy functions implemented in JavaScript>
--
module Data.Geo.Jord
    ( module Data.Geo.Jord.Angle
    , module Data.Geo.Jord.Duration
    , module Data.Geo.Jord.Ellipsoid
    , module Data.Geo.Jord.Ellipsoids
    , module Data.Geo.Jord.LocalFrames
    , module Data.Geo.Jord.Geodesic
    , module Data.Geo.Jord.GreatCircle
    , module Data.Geo.Jord.Kinematics
    , module Data.Geo.Jord.LatLong
    , module Data.Geo.Jord.Length
    , module Data.Geo.Jord.Model
    , module Data.Geo.Jord.Models
    , module Data.Geo.Jord.Position
    , module Data.Geo.Jord.Quantity
    , module Data.Geo.Jord.Rotation
    , module Data.Geo.Jord.Speed
    , module Data.Geo.Jord.Transformation
    , module Data.Geo.Jord.Transformations
    , module Data.Geo.Jord.Vector3d
    , jordVersion
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Duration
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Ellipsoids
import Data.Geo.Jord.Geodesic
import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.Kinematics
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.LocalFrames
import Data.Geo.Jord.Model
import Data.Geo.Jord.Models
import Data.Geo.Jord.Position
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Rotation
import Data.Geo.Jord.Speed
import Data.Geo.Jord.Transformation
import Data.Geo.Jord.Transformations
import Data.Geo.Jord.Vector3d

-- | version.
jordVersion :: String
jordVersion = "1.0.0.0"
