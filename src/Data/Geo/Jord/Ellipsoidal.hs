module Data.Geo.Jord.Ellipsoidal
    ( EGeodetics(delta, destination, geodeticHeight)
    ) where

import Data.Geo.Jord.Ellipsoidal.NedVector
import Data.Geo.Jord.Positions

-- class ETransform a where

class EGeodetics a where
    delta :: GeoPos a -> GeoPos a -> NedVector
    destination :: GeoPos a -> NedVector -> GeoPos a -- TODO find other name
    geodeticHeight :: GeoPos a -> Double
