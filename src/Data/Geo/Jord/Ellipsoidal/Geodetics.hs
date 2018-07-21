{-# LANGUAGE MultiParamTypeClasses #-}

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

import Data.Geo.Jord.Position
import Data.Geo.Jord.Ellipsoidal.NedVector

exactDelta :: (GeographicPosition a) => a -> a -> NedVector
exactDelta = undefined

exactDestination :: (GeographicPosition a) => a -> NedVector -> a
exactDestination = undefined
