module Data.Geo.Jord.Geodesic
    ( direct
    , inverse
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Bodies
import Data.Geo.Jord.Length
import Data.Geo.Jord.Position

direct :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a, Angle)
direct _ _ _ = Nothing

inverse :: (Ellipsoidal a) => Position a -> Position a -> Maybe (Length, Angle, Angle)
inverse _ _ = Nothing
