-- |
-- Module:      Data.Geo.Jord.Geodesic
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- TODO
--
module Data.Geo.Jord.Geodesic
    ( directGeodesic
    , inverseGeodesic
    , geodesicDistance
    , geodesicDestination
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Length
import Data.Geo.Jord.Model
import Data.Geo.Jord.Position

directGeodesic :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a, Angle)
directGeodesic _ _ _ = Nothing

inverseGeodesic :: (Ellipsoidal a) => Position a -> Position a -> Maybe (Length, Angle, Angle)
inverseGeodesic _ _ = Nothing

geodesicDistance :: (Ellipsoidal a) => Position a -> Position a -> Maybe Length
geodesicDistance _ _ = Nothing

geodesicDestination :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a)
geodesicDestination _ _ _ = Nothing