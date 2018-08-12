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
    , latLongHeight
    , decimalLatLongHeight
    , decimalLatLongHeightE
    , decimalLatLongHeightF
    , nvectorHeight
    ) where

import Control.Monad.Fail
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector

-- | An earth position defined by an horizontal position and height.
--
-- horizontal position can be either a 'LatLong' or a 'NVector'.
data AngularPosition a = AngularPosition
    { pos :: a
    , height :: Length
    } deriving (Eq, Show)

-- | 'AngularPosition' from a 'LatLong' and height.
latLongHeight :: LatLong -> Length -> AngularPosition LatLong
latLongHeight = AngularPosition

-- | 'AngularPosition' from given latitude and longitude in __decimal degrees__ and height.
-- 'error's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
decimalLatLongHeight :: Double -> Double -> Length -> AngularPosition LatLong
decimalLatLongHeight lat lon = latLongHeight (decimalLatLong lat lon)

-- | 'AngularPosition' from given latitude and longitude in __decimal degrees__ and height.
-- A 'Left' indicates that the given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
decimalLatLongHeightE :: Double -> Double -> Length -> Either String (AngularPosition LatLong)
decimalLatLongHeightE lat lon h = fmap (`latLongHeight` h) (decimalLatLongE lat lon)

-- | 'AngularPosition' from given latitude and longitude in __decimal degrees__ and height.
-- 'fail's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
decimalLatLongHeightF :: (MonadFail m) => Double -> Double -> Length -> m (AngularPosition LatLong)
decimalLatLongHeightF lat lon h = fmap (`latLongHeight` h) (decimalLatLongF lat lon)

-- | 'AngularPosition' from a 'NVector' and height.
nvectorHeight :: NVector -> Length -> AngularPosition NVector
nvectorHeight = AngularPosition
