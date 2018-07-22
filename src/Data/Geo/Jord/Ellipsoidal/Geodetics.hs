-- |
-- Module:      Data.Geo.Jord.Ellipsoidal.Geodetics
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geodetics calculations on positions assuming an ellipsoidal earth model.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Ellipsoidal.Geodetics
   (
     exactDelta
   , exactDestination
   ) where

import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Position
import Data.Geo.Jord.Ellipsoidal.NedVector

exactDelta :: (GeographicPosition a) => a -> a -> NedVector
exactDelta p1 p2 = nedVector (metres (nx r)) (metres (ny r)) (metres (nz r))
    where
        ep1 = toEcefPosition p1
        ep2 = toEcefPosition p2
        dpe = nvector (ex ep2 - ex ep1) (ey ep2 - ey ep1) (ez ep2 - ez ep1)
        n1 = getNVector (toNVectorPosition p1)
        np = northPole
        d = scale n1 (-1) -- down (pointing opposite to n-vector)
        e = unit (cross np n1) -- east (pointing perpendicular to the plane)
        n = cross e d -- north (by right hand rule)
        r = rotate dpe [n, e, d]

exactDestination :: (GeographicPosition a) => a -> NedVector -> a
exactDestination = undefined
