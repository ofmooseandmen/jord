-- |
-- Module:      Data.Geo.Jord.AngularPosition
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Angular positions.
--
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
module Data.Geo.Jord.AngularPosition
    ( AngularPosition(..)
    , latLongPos
    , nvectorPos
    ) where

import Data.Geo.Jord.LatLong
import Data.Geo.Jord.NVector

-- | An earth position defined by an horizontal position and height.
--
-- horizontal position can be either a 'LatLong' or a 'NVector'.
data AngularPosition a = AngularPosition
    { pos :: a
    , height :: Double
    } deriving (Eq, Show)

-- | 'AngularPosition' from a 'LatLong' and height.
latLongPos :: LatLong -> Double -> AngularPosition LatLong
latLongPos = AngularPosition

-- | 'AngularPosition' from a 'NVector' and height.
nvectorPos :: NVector -> Double -> AngularPosition NVector
nvectorPos = AngularPosition
