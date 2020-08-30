module Data.Geo.Jord.Geocentric
    ( Coordinates(..)
    , toMetres
    , metres
    , Position(..)
    , metresPos
    , antipode
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres, toMetres)
import Data.Geo.Jord.Math3d (V3(..))
import Data.Geo.Jord.Model

toMetres :: Coordinates -> V3
toMetres (Coordinates x' y' z') = V3 (Length.toMetres x') (Length.toMetres y') (Length.toMetres z')

metres :: Double -> Double -> Double -> Coordinates
metres xm ym zm = Coordinates (Length.metres xm) (Length.metres ym) (Length.metres zm)

metresPos :: (Model a) => Double -> Double -> Double -> a -> Position a
metresPos xm ym zm = Position (metres xm ym zm)

data Coordinates =
    Coordinates
        { x :: Length
        , y :: Length
        , z :: Length
        } deriving (Eq, Show)

data Position a =
    Position
        { coords :: Coordinates
        , model :: a
        }  deriving (Eq, Show)

-- FIXME implement 3 functions below
antipode :: (Model a) => Position a -> Position a
antipode p = p

northPole :: (Model a) => a -> Position a
northPole = Position (metres 0 0 0)

southPole :: (Model a) => a -> Position a
southPole = Position (metres 0 0 0)
