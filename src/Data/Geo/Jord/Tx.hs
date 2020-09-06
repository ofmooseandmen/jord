-- |
-- Module:      Data.Geo.Jord.Tx
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Coordinates transformation parameters.
module Data.Geo.Jord.Tx
    (
    -- * transformation parameters.
      Tx(..)
    , inverse
    , Params(..)
    , Params7
    , Rates
    , Params15(..)
    , params7
    , rates
    , paramsAt
    -- * transformation graph.
    , Graph
    , graph
    , paramsBetween
    -- * geocentric coordinate transformation
    , apply
    ) where

import Data.List (find, foldl', sortOn)
import Data.Maybe (mapMaybe)

import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Epoch(..), ModelId)

-- | Coordinate transformation between 2 models (A & B).
data Tx a =
    Tx
        { modelA :: ModelId -- ^  model A.
        , modelB :: ModelId -- ^ model B.
        , params :: a -- ^ transformation parameters - i.e. 'modelA'-> 'modelB'
        }

-- | inverse transformation.
inverse :: (Params a) => Tx a -> Tx a
inverse t = Tx (modelB t) (modelA t) (inverseParams (params t))

-- | class for transformation parameters.
class Params a where
    idParams :: a -- ^ identity transformation parameters, i.e. @p T idParams = p@.
    inverseParams :: a -> a -- ^ inverse transformation parameters.

-- | 7-parameter transformation (Helmert); use 'params7' to construct.
data Params7 =
    Params7 !Math3d.V3 !Double !Math3d.V3
    deriving (Show)

instance Params Params7 where
    idParams = Params7 Math3d.zero 0 Math3d.zero
    inverseParams (Params7 c s r) = Params7 (Math3d.scale c (-1.0)) (-s) (Math3d.scale r (-1.0))

instance Params Params15 where
    idParams = Params15 (Epoch 0) idParams (Rates Math3d.zero 0 Math3d.zero)
    inverseParams (Params15 e p (Rates c s r)) =
        Params15 e (inverseParams p) (Rates (Math3d.scale c (-1.0)) (-s) (Math3d.scale r (-1.0)))

-- | Transformation rates for the 15-parameter transformation (Helmert); use 'rates' to construct.
data Rates =
    Rates !Math3d.V3 !Double !Math3d.V3
    deriving (Show)

-- | Epoch and 14-parameter transformation (Helmert).
data Params15 =
    Params15 Epoch Params7 Rates
    deriving (Show)

-- | 7-parameter transformation (Helmert) from given translation vector, scale factor and rotation matrix.
params7 ::
       (Double, Double, Double) -- ^ translation vector containing the three translations along the coordinate axes: tx, ty, tz in __millimetres__
    -> Double -- ^ scale factor (unitless) expressed in __parts per billion__
    -> (Double, Double, Double) -- ^ rotation matrix (orthogonal) consisting of the three axes rx, ry, rz in __milliarcseconds__
    -> Params7
params7 c s r = Params7 (mmToMetres c) (s / 1e9) (masToRadians r)

-- | rates of the 15-parameter translation (Helmert) from given translation rates, scale factor rate and rotation rates.
rates ::
       (Double, Double, Double) -- ^ translation rate in __millimetres per year__.
    -> Double -- ^ scale factor rate in __part per billion per year__.
    -> (Double, Double, Double) -- ^ rotation rate in __milliarcseconds per year__.
    -> Rates
rates c s r = Rates (mmToMetres c) (s / 1e9) (masToRadians r)

mmToMetres :: (Double, Double, Double) -> Math3d.V3
mmToMetres (cx, cy, cz) = Math3d.scale (Math3d.vec3 cx cy cz) (1.0 / 1000.0)

masToRadians :: (Double, Double, Double) -> Math3d.V3
masToRadians (rx, ry, rz) = Math3d.scale (Math3d.vec3 rx ry rz) (pi / (3600.0 * 1000.0 * 180.0))

-- | @paramsAt e tx15@ returns the 7-parameter transformation corresponding to the
-- 15-parameter transformation @tx15@ at epoch @e@.
paramsAt :: Epoch -> Params15 -> Params7
paramsAt (Epoch e) (Params15 (Epoch pe) (Params7 c s r) (Rates rc rs rr)) = Params7 c' s' r'
  where
    de = e - pe
    c' = Math3d.add c (Math3d.scale rc de)
    s' = s + de * rs
    r' = Math3d.add r (Math3d.scale rr de)

-- | node to adjacent nodes.
data Connection =
    Connection
        { node :: !ModelId
        , adjacents :: ![ModelId]
        }

-- | graph edge: from model, tx params, to model.
data Edge a =
    Edge ModelId a ModelId

-- path of visited models.
type Path = [ModelId]

-- queued, visited.
data State =
    State [ModelId] [Path]

-- | Transformation graph: vertices are 'ModelId' and edges are transformation parameters.
data Graph a =
    Graph ![Connection] ![Edge a]

-- | @graph ts@ returns a transformation graph containing all given direct and inverse
-- (i.e. for each 'Tx': 'params' & 'inverseParams') transformations.
graph :: (Params a) => [Tx a] -> Graph a
graph = foldl' addTx emptyGraph

-- | @paramsBetween m0 m1 g@ computes the ordered list of transformation parameters to be
-- successively applied when transforming the coordinates of a position in model @m0@ to model @m1@.
-- The returned list is empty, if either model is not in the graph (i.e. not a vertex)  or if no
-- such transformation exists (i.e. model @m1@ cannot be reached from model @m0@).
paramsBetween :: (Params a) => ModelId -> ModelId -> Graph a -> [a]
paramsBetween m0 m1 g
    | m0 == m1 = [idParams]
    | null ms = []
    | otherwise = findParams ms g
  where
    ms = dijkstra (State [m0] []) m1 g

-- | empty graph.
emptyGraph :: Graph a
emptyGraph = Graph [] []

-- | add 'Tx' to graph.
addTx :: (Params a) => Graph a -> Tx a -> Graph a
addTx (Graph cs es) t = Graph cs' es'
  where
    ma = modelA t
    mb = modelB t
    cs1 = addConnection cs ma mb
    cs' = addConnection cs1 mb ma
    txp = params t
    es' = Edge ma txp mb : Edge mb (inverseParams txp) ma : es

-- | add connection to graph.
addConnection :: [Connection] -> ModelId -> ModelId -> [Connection]
addConnection cs m1 m2
    | null filtered = Connection m1 [m2] : cs
    | otherwise =
        map
            (\c' ->
                 if node c' == m1
                     then updated
                     else c')
            cs
  where
    filtered = filter (\c -> node c == m1) cs
    cur = head filtered
    updated = cur {adjacents = m2 : adjacents cur}

-- | successors of given model in given graph.
successors :: ModelId -> Graph a -> [ModelId]
successors m (Graph cs _) = concatMap adjacents (filter (\c -> node c == m) cs)

-- | visit every given model from given model.
visit :: ModelId -> [ModelId] -> State -> State
visit f ms (State q0 v0) = State q1 v1
  where
    toVisit = filter (`notElem` concat v0) ms -- filter models already visited
    fs = filter (\v -> head v == f) v0 -- all paths starting at f
    q1 = q0 ++ toVisit
    updatedPaths = concatMap (\x -> map (: x) toVisit) fs
    v1 = updatedPaths ++ filter (\v -> head v /= f) v0

shortest :: ModelId -> ModelId -> [Path] -> [ModelId]
shortest c m ps = reverse (m : s)
  where
    fs = filter (\v -> head v == c) ps -- all paths starting at c
    s = head (sortOn length fs)

-- | dijkstra.
dijkstra :: State -> ModelId -> Graph a -> [ModelId]
dijkstra (State [] _) _ _ = []
dijkstra (State [c] []) t g = dijkstra (State [c] [[c]]) t g
dijkstra (State (c:r) v) t g
    | t `elem` succs = shortest c t v
    | otherwise = dijkstra s'' t g
  where
    s' = State r v
    succs = successors c g
    s'' = visit c succs s'

-- | find tx params between given models: [A, B, C] => params (A, B), params (B, C).
findParams :: [ModelId] -> Graph a -> [a]
findParams ms (Graph _ es)
    | length ps == length r = r
    | otherwise = []
  where
    ps = zip ms (tail ms)
    r = mapMaybe (`findParam` es) ps

-- | find tx params between (A, B).
findParam :: (ModelId, ModelId) -> [Edge a] -> Maybe a
findParam p es = fmap (\(Edge _ pa _) -> pa) (find (edgeEq p) es)

-- | edge eq given pair?
edgeEq :: (ModelId, ModelId) -> Edge a -> Bool
edgeEq (m1, m2) (Edge m1' _ m2') = m1 == m1' && m2 == m2'

-- | @apply gc tx7@ returns the geocentric coordinates resulting from applying the 7-parameter
-- transformation @tx7@ to the geocentric coordinates represented by vector @gc@.
apply :: Math3d.V3 -> Params7 -> Math3d.V3
apply gc (Params7 c s r) = Math3d.add c (Math3d.scale (Math3d.multM gc (rotation r)) (1.0 + s))

rotation :: Math3d.V3 -> [Math3d.V3]
rotation v = [Math3d.vec3 1.0 (-z) y, Math3d.vec3 z 1.0 (-x), Math3d.vec3 (-y) x 1.0]
  where
    x = Math3d.v3x v
    y = Math3d.v3y v
    z = Math3d.v3z v
