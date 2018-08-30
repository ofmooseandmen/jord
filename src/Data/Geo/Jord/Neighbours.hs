-- |
-- Module:      Data.Geo.Jord.Neigbours
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for doing the nearsest neighbour search on a set of points
-- assuming a __spherical__ earth model.
--
module Neighbours
    ( Space
    , space
    , space84
    , surfaceNearestNeighbours
    , surfaceNeighbours
    ) where

import Data.Geo.Jord.Earth (r84)
import Data.Geo.Jord.Length
import Data.Geo.Jord.Transformation
import Data.Geo.Jord.Vector3d
import Data.List (find)

-- | Search space: a set of points on the surface of the earth, stored in
-- a <https://en.wikipedia.org/wiki/Vantage-point_tree Vantage Point Tree>.
data Space a
    = Empty
    | Root (Node a)

data Neighbour a = Neighbour
    { position :: a
    , separation :: Length
    } deriving (Show, Eq)

data Node a
    = Leaf a
    | Child { vp :: (a, Vector3d)
            , mu :: Double
            , inside :: Maybe (Node a)
            , outside :: Maybe (Node a) }

newtype Cache = Cache [((Vector3d, Vector3d), Double)]

space :: (NTransform a) => [a] -> Length -> Space a
space [] _ = Empty
space ps r = maybe Empty Root (node ps r)

node :: (NTransform a) => [a] -> Length -> Maybe (Node a)
node [] _ = Nothing
node ps r = Nothing

space84 :: (NTransform a) => [a] -> Space a
space84 ps = space ps r84

surfaceNearestNeighbours :: (NTransform a) => a -> Int -> Space a -> [Neighbour a]
surfaceNearestNeighbours p k s = []

surfaceNeighbours :: (NTransform a) => a -> Length -> Space a -> [Neighbour a]
surfaceNeighbours p m s = []

sd :: Vector3d -> Vector3d -> Double -> Cache -> (Double, Cache)
sd a b r c@(Cache es) =
    case cd of
        Nothing -> (sep, Cache (es ++ [((a, b), sep)]))
        (Just s) -> (s, c)
  where
    cd = fmap snd (find (\e -> fst e == (a, b)) es)
    sep = ad a b * r

-- | angle in radians between 2 n-vectors (as vector3d), copied from Geodetics
-- without the sign and returing radians.
-- FIXME: move to internal module.
ad :: Vector3d -> Vector3d -> Double
ad v1 v2 = atan2 (vnorm (vcross v1 v2)) (vdot v1 v2)
