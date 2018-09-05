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
module Data.Geo.Jord.Neighbours
    ( Space
    , optimalSpace
    , optimalSpace84
    , surfaceNearestNeighbours
    , surfaceNeighbours
    ) where

import Data.Function (on)
import Data.Geo.Jord.Earth (r84)
import Data.Geo.Jord.Internal (nvec, sdist)
import Data.Geo.Jord.Length
import Data.Geo.Jord.Transformation (IsNVector(..))
import Data.Geo.Jord.Vector3d
import Data.List (maximumBy, partition, sort)

-- | Search space: a set of points on the surface of the earth, stored in
-- a <https://en.wikipedia.org/wiki/Vantage-point_tree Vantage Point Tree>.
data Space a
    = Empty
    | Root (Node a)
    deriving (Show)

data Neighbour a = Neighbour
    { position :: a
    , separation :: Length
    } deriving (Show, Eq)

data Node a
    = Leaf (Pt a)
    | Child { vp :: Pt a
            , mu :: Double
            , inside :: Maybe (Node a)
            , outside :: Maybe (Node a) }
    deriving (Show)

-- | Builds an optimal space by selecting the best vantage point at each
-- partitioning step. The best vantage point is the point that is the
-- further away from all other points. Time complexity is O(n^2) which makes
-- it usable only for thousands of points. Search time is however optimal.
optimalSpace :: (IsNVector a) => [a] -> Length -> Space a
optimalSpace [] _ = Empty
optimalSpace ps r = maybe Empty Root (node (pts ps) (toMetres r))

optimalSpace84 :: (IsNVector a) => [a] -> Space a
optimalSpace84 ps = optimalSpace ps r84

surfaceNearestNeighbours :: (IsNVector a) => a -> Int -> Space a -> [Neighbour a]
surfaceNearestNeighbours p k s = []

surfaceNeighbours :: (IsNVector a) => a -> Length -> Space a -> [Neighbour a]
surfaceNeighbours p m s = []

-- Private part
data Pt a = Pt
    { pt :: a
    , nv :: Vector3d
    } deriving (Show)

data Vp a = Vp
    { vpt :: Pt a
    , median :: Double
    , spread :: Double
    , rest :: [(Pt a, Double)]
    }

node :: [Pt a] -> Double -> Maybe (Node a)
node [] _ = Nothing
node [a] _ = Just (Leaf a)
node ps r = Just (Child (vpt v) (median v) (node (map fst ins) r) (node (map fst outs) r))
  where
    v = selectVp ps r
    (ins, outs) = partition (\p -> snd p < median v) (rest v)

pts :: (IsNVector a) => [a] -> [Pt a]
pts = map (\x -> Pt x (nvec x))

-- | select optimal vantage point amongts given points.
selectVp :: [Pt a] -> Double -> Vp a
selectVp xs r = maximumBy (compare `on` spread) (pVps xs r)

-- | potential vantage points.
pVps :: [Pt a] -> Double -> [Vp a]
pVps xs r = map (`pVp` r) (tuples xs)

-- | potential vantage point.
pVp :: (Pt a, [Pt a]) -> Double -> Vp a
pVp (x, xs) r = Vp x m s (zip xs ds)
  where
    ds = distances x xs r
    (m, s) = stats ds

distances :: Pt a -> [Pt a] -> Double -> [Double]
distances (Pt _ v) ps r = map (\p -> sdist v (nv p) r) ps

-- | [1, 2, 3, 4, 5] -> [(1,[2,3,4,5]),(2,[1,3,4,5]),(3,[1,2,4,5]),(4,[1,2,3,5]),(5,[1,2,3,4])]
tuples :: [a] -> [(a, [a])]
tuples xs = map (\i -> (xs !! i, tuple i)) [0 .. length xs - 1]
  where
    tuple i = take i xs ++ drop (i + 1) xs

-- | median  and spread of given sorted list.
stats :: [Double] -> (Double, Double)
stats xs = (m, foldl (\a b -> a + ((b - m) * (b - m))) 0 xs)
  where
    m = med xs

-- | median of given list.
med :: [Double] -> Double
med xs
    | odd len = sxs !! mid
    | otherwise = em
  where
    sxs = sort xs
    len = length xs
    mid = len `quot` 2
    em = (sxs !! mid + sxs !! (mid - 1)) / 2.0
