module Data.Geo.Jord.Ellipsoidal
    ( EGeodetics(delta, endPosition, geodeticHeight)
    ) where

import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.NedVector
import Data.Geo.Jord.Positions

class EGeodetics a where
    delta :: GeoPos a Ellipsoid -> GeoPos a Ellipsoid -> NedVector
    endPosition :: GeoPos a Ellipsoid -> NedVector -> GeoPos a Ellipsoid -- TODO find a better name
    geodeticHeight :: GeoPos a Ellipsoid -> Double
