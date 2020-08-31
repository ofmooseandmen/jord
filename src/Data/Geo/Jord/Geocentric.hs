module Data.Geo.Jord.Geocentric
    ( Position(..)
    , coords
    , metresCoords
    , metresPos
    , antipode
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres, toMetres)
import Data.Geo.Jord.Math3d (V3(..))
import Data.Geo.Jord.Model

data Position a =
    Position
        { gx :: Length
        , gy :: Length
        , gz :: Length
        , model :: a
        }
    deriving (Eq, Show)

metresCoords :: (Model a) => Position a -> V3
metresCoords p = coords p Length.toMetres

coords :: (Model a) => Position a -> (Length -> Double) -> V3
coords (Position x y z _) f = V3 (f x) (f y) (f z)

metresPos :: (Model a) => Double -> Double -> Double -> a -> Position a
metresPos xm ym zm = Position (Length.metres xm) (Length.metres ym) (Length.metres zm)

-- FIXME implement 3 functions below
antipode :: (Model a) => Position a -> Position a
antipode p = p

northPole :: (Model a) => a -> Position a
northPole = metresPos 0 0 0

southPole :: (Model a) => a -> Position a
southPole = metresPos 0 0 0
