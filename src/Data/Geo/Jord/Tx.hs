-- |
-- Module:      Data.Geo.Jord.Transformation
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Coordinates transformation parameters.
--
module Data.Geo.Jord.Tx
    (
    -- * transformation parameters.
      Tx(..)
    , inverseTx
    , TxParams(..)
    , TxParams7
    , TxRates
    , TxParams15(..)
    , txParams7
    , txRates
    , txParamsAt
    -- * transformation graph.
    , TxGraph
    , txGraph
    , txParamsBetween
    -- * geocentric coordinate transformation
    , transformGeoc
    ) where

import Data.List (find, foldl', sortOn)
import Data.Maybe (mapMaybe)

import Data.Geo.Jord.Model
import Data.Geo.Jord.Vector3d

-- | Coordinate transformation between 2 models (A & B).
data Tx a =
    Tx
        { modelA :: ModelId -- ^  model A.
        , modelB :: ModelId -- ^ model B.
        , txParams :: a -- ^ transformation parameters - i.e. 'modelA'-> 'modelB'
        }

-- | inverse transformation.
inverseTx :: (TxParams a) => Tx a -> Tx a
inverseTx t = Tx (modelB t) (modelA t) (inverseTxParams (txParams t))

-- | class for transformation parameters.
class TxParams a where
    idTxParams :: a -- ^ identity transformation parameters, i.e. @p T idTxParams = p@.
    inverseTxParams :: a -> a -- ^ inverse transformation parameters.

-- | 7-parameter transformation (Helmert); use 'txParams7' to construct.
data TxParams7 =
    TxParams7 !Vector3d !Double !Vector3d
    deriving (Show)

instance TxParams TxParams7 where
    idTxParams = TxParams7 (Vector3d 0 0 0) 0 (Vector3d 0 0 0)
    inverseTxParams (TxParams7 c s r) = TxParams7 (vscale c (-1.0)) (-s) (vscale r (-1.0))

instance TxParams TxParams15 where
    idTxParams = TxParams15 (Epoch 0) idTxParams (TxRates (Vector3d 0 0 0) 0 (Vector3d 0 0 0))
    inverseTxParams (TxParams15 e p (TxRates c s r)) =
        TxParams15 e (inverseTxParams p) (TxRates (vscale c (-1.0)) (-s) (vscale r (-1.0)))

-- | Transformation rates for the 15-parameter transformation (Helmert); use 'txRates' to construct.
data TxRates =
    TxRates !Vector3d !Double !Vector3d
    deriving (Show)

-- | Epoch and 14-parameter transformation (Helmert).
data TxParams15 =
    TxParams15 Epoch TxParams7 TxRates
    deriving (Show)

-- | 7-parameter transformation (Helmert) from given translation vector, scale factor and rotation matrix.
txParams7 ::
       (Double, Double, Double) -- ^ translation vector containing the three translations along the coordinate axes: tx, ty, tz in __millimetres__
    -> Double -- ^ scale factor (unitless) expressed in __parts per billion__
    -> (Double, Double, Double) -- ^ rotation matrix (orthogonal) consisting of the three axes rx, ry, rz in __milliarcseconds__
    -> TxParams7
txParams7 c s r = TxParams7 (mmToMetres c) (s / 1e9) (masToRadians r)

-- | rates of the 15-parameter translation (Helmert) from given translation rates, scale factor rate and rotation rates.
txRates ::
       (Double, Double, Double) -- ^ translation rate in __millimetres per year__.
    -> Double -- ^ scale factor rate in __part per billion per year__.
    -> (Double, Double, Double) -- ^ rotation rate in __milliarcseconds per year__.
    -> TxRates
txRates c s r = TxRates (mmToMetres c) (s / 1e9) (masToRadians r)

mmToMetres :: (Double, Double, Double) -> Vector3d
mmToMetres (cx, cy, cz) = vscale (Vector3d cx cy cz) (1.0 / 1000.0)

masToRadians :: (Double, Double, Double) -> Vector3d
masToRadians (rx, ry, rz) = vscale (Vector3d rx ry rz) (pi / (3600.0 * 1000.0 * 180.0))

-- | @txParamsAt e tx15@ returns the 7-parameter transformation corresponding to the
-- 15-parameter transformation @tx15@ at epoch @e@.
txParamsAt :: Epoch -> TxParams15 -> TxParams7
txParamsAt (Epoch e) (TxParams15 (Epoch pe) (TxParams7 c s r) (TxRates rc rs rr)) =
    TxParams7 c' s' r'
  where
    de = e - pe
    c' = vadd c (vscale rc de)
    s' = s + de * rs
    r' = vadd r (vscale rr de)

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
data TxGraph a =
    TxGraph ![Connection] ![Edge a]

-- | @txGraph ts@ returns a transformation graph containing all given direct and inverse
-- (i.e. for each 'Tx': 'txParams' & 'inverseTxParams') transformations.
txGraph :: (TxParams a) => [Tx a] -> TxGraph a
txGraph = foldl' addTx emptyGraph

-- | @txParamsBetween m0 m1 g@ computes the ordered list of transformation parameters to be
-- successively applied when transforming the coordinates of a position in model @m0@ to model @m1@.
-- The returned list is empty, if either model is not in the graph (i.e. not a vertex)  or if no
-- such transformation exists (i.e. model @m1@ cannot be reached from model @m0@).
txParamsBetween :: (TxParams a) => ModelId -> ModelId -> TxGraph a -> [a]
txParamsBetween m0 m1 g
    | m0 == m1 = [idTxParams]
    | null ms = []
    | otherwise = findParams ms g
  where
    ms = dijkstra (State [m0] []) m1 g

-- | empty graph.
emptyGraph :: TxGraph a
emptyGraph = TxGraph [] []

-- | add 'Tx' to graph.
addTx :: (TxParams a) => TxGraph a -> Tx a -> TxGraph a
addTx (TxGraph cs es) t = TxGraph cs' es'
  where
    ma = modelA t
    mb = modelB t
    cs1 = addConnection cs ma mb
    cs' = addConnection cs1 mb ma
    txp = txParams t
    es' = Edge ma txp mb : Edge mb (inverseTxParams txp) ma : es

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
successors :: ModelId -> TxGraph a -> [ModelId]
successors m (TxGraph cs _) = concatMap adjacents (filter (\c -> node c == m) cs)

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
dijkstra :: State -> ModelId -> TxGraph a -> [ModelId]
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
findParams :: [ModelId] -> TxGraph a -> [a]
findParams ms (TxGraph _ es)
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

-- | @transformGeoc gc tx7@ returns the geocentric coordinates resulting from applying the 7-parameter
-- transformation @tx7@ to the geocentric coordinates represented by vector @gc@.
transformGeoc :: Vector3d -> TxParams7 -> Vector3d
transformGeoc gc (TxParams7 c s r) = vadd c (vscale (vmultm gc (rotation r)) (1.0 + s))

rotation :: Vector3d -> [Vector3d]
rotation (Vector3d x y z) = [Vector3d 1.0 (-z) y, Vector3d z 1.0 (-x), Vector3d (-y) x 1.0]
