-- |
-- Module:      Data.Geo.Jord.Transformation
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Coordinates transformation between ellipsoidal models.
--
module Data.Geo.Jord.Transformation
    (
    -- * transformation parameters.
      Tx(..)
    , TxParams(..)
    , TxParams7
    , TxRates
    , TxParams15(..)
    , txParams7
    , txRates
    -- * transformation graph.
    , TxGraph
    , txGraph
    , txParamsBetween
    -- * coordinates transformation.
    , transformCoords
    , transformCoords'
    , transformCoordsAt
    , transformCoordsAt'
    ) where

import Data.List (find, foldl', sortOn)
import Data.Maybe (mapMaybe)

import Data.Geo.Jord.Model
import Data.Geo.Jord.Position
import Data.Geo.Jord.Vector3d

-- | Coordinate transformation between 2 models (A & B).
data Tx a =
    Tx
        { modelA :: ModelId -- ^  model A.
        , modelB :: ModelId -- ^ model B.
        , txParams :: a -- ^ transformation parameters - i.e. 'modelA'-> 'modelB'
        }

-- | class for transformation parameters.
class TxParams a where
    idTxParams :: a -- ^ identity transformation, i.e. @p T idTxParams = p@.
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

-- | @transformCoords p1 m2 g@ transforms the coordinates of the position @p1@ from its coordinate
-- system into the coordinate system defined by the model @m2@ using the graph @g@ to find the
-- sequence of transformation parameters. Returns 'Nothing' if the given graph does not contain a
-- transformation from @m1@ to @m2@ - see 'txParamsBetween'.
--
-- ==== __Examples__
--
-- >>> let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
-- >>> transformCoords pWGS84 NAD83 staticTxs
-- >>> Just 48°41'31.523"N,6°11'3.723"E 188.1212m (NAD83)
--
transformCoords ::
       (Ellipsoidal a, Ellipsoidal b) => Position a -> b -> TxGraph TxParams7 -> Maybe (Position b)
transformCoords p1 m2 g = transformCoordsF p1 m2 g id

-- | @transformCoords' p1 m2 tx@ transforms the coordinates of the position @p1@ from its coordinate system
-- into the coordinate system defined by the model @m2@ using the 7-parameters transformation @tx@.
--
-- Notes: this function does not checks whether both models are equals. It should be used when the
-- 7-parameter transformation is known. Most of the time prefer using 'transformCoords'.
--
-- ==== __Examples__
--
-- >>> let tx = txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599) -- WGS84 -> NAD83
-- >>> let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
-- >>> transformCoords' pWGS84 NAD83 tx
-- >>> 48°41'31.523"N,6°11'3.723"E 188.1212m (NAD83)
--
transformCoords' :: (Ellipsoidal a, Ellipsoidal b) => Position a -> b -> TxParams7 -> Position b
transformCoords' = transformPosCoords

-- | @transformCoordsAt p1 e m2 g@ transforms the coordinates of the position @p1@ observed at epoch @e@
-- from its coordinate system into the coordinate system defined by the model @m2@ using the graph @g@ to
-- find the sequence of transformation parameters. Returns 'Nothing' if the given graph does not contain a
-- transformation from @m1@ to @m2@ - see 'txParamsBetween'.
--
-- ==== __Examples__
--
-- >>> let pITRF2014 = latLongHeightPos 48.6921 6.1844 (metres 188) ITRF2014
-- >>> transformCoordsAt pITRF2014 (Epoch 2019.0) NAD83_CORS96 dynamicTxs -- through ITRF2000
-- >>> Just 48°41'31.538"N,6°11'3.722"E 188.112035m (NAD83_CORS96)
--
transformCoordsAt ::
       (EllipsoidalT0 a, EllipsoidalT0 b)
    => Position a
    -> Epoch
    -> b
    -> TxGraph TxParams15
    -> Maybe (Position b)
transformCoordsAt p1 e m2 g = transformCoordsF p1 m2 g (txParamsAt e)

-- | @transformCoordsAt' p1 e m2 tx@ transforms the coordinates of the position @p1@ observed at epoch @e@
-- from its coordinate system into the coordinate system defined by the model @m2@ using
-- the 15-parameters transformation @tx@.
--
-- Notes: this function does not checks whether both models are equals. It should be used when the
-- 15-parameter transformation is known. Most of the time prefer using 'transformCoords'.
--
-- ==== __Examples__
--
-- >>> let tx7 = txParams7 (53.7, 51.2, -55.1) 1.2 (0.891, 5.39, -8.712)
-- >>> let txR = txRates (0.1, 0.1, -1.9) 0.11 (0.81, 0.49, -0.792)
-- >>> let tx = TxParams15 (Epoch 2000.0) tx7 txR -- ITRF2014 -> ETRF2000
-- >>> let pITRF2014 = latLongHeightPos 48.6921 6.1844 (metres 188) ITRF2014
-- >>> transformCoordsAt' pITRF2014 (Epoch 2019.0) ETRF2000 tx
-- >>> 48°41'31.561"N,6°11'3.865"E 188.0178m (ETRF2000)
--
transformCoordsAt' ::
       (EllipsoidalT0 a, EllipsoidalT0 b) => Position a -> Epoch -> b -> TxParams15 -> Position b
transformCoordsAt' p1 e m2 ps = transformPosCoords p1 m2 (txParamsAt e ps)

transformCoordsF ::
       (Ellipsoidal a, Ellipsoidal b, TxParams p)
    => Position a
    -> b
    -> TxGraph p
    -> (p -> TxParams7)
    -> Maybe (Position b)
transformCoordsF p1 m2 g f =
    case ps of
        [] -> Nothing
        _ -> Just (geocentricMetresPos v2x v2y v2z m2)
  where
    mi1 = modelId . model $ p1
    mi2 = modelId m2
    ps = txParamsBetween mi1 mi2 g
    (Vector3d v2x v2y v2z) = foldl (\gc p -> transformGeoc gc (f p)) (gcvec p1) ps

transformPosCoords :: (Model a, Model b) => Position a -> b -> TxParams7 -> Position b
transformPosCoords p1 m2 ps = geocentricMetresPos v2x v2y v2z m2
  where
    (Vector3d v2x v2y v2z) = transformGeoc (gcvec p1) ps

-- | geocentric coordinates transformation.
transformGeoc :: Vector3d -> TxParams7 -> Vector3d
transformGeoc gc (TxParams7 c s r) = vadd c (vscale (vmultm gc (rotation r)) (1.0 + s))

-- | tx parameters at epoch.
txParamsAt :: Epoch -> TxParams15 -> TxParams7
txParamsAt (Epoch e) (TxParams15 (Epoch pe) (TxParams7 c s r) (TxRates rc rs rr)) =
    TxParams7 c' s' r'
  where
    de = e - pe
    c' = vadd c (vscale rc de)
    s' = s + de * rs
    r' = vadd r (vscale rr de)

mmToMetres :: (Double, Double, Double) -> Vector3d
mmToMetres (cx, cy, cz) = vscale (Vector3d cx cy cz) (1.0 / 1000.0)

masToRadians :: (Double, Double, Double) -> Vector3d
masToRadians (rx, ry, rz) = vscale (Vector3d rx ry rz) (pi / (3600.0 * 1000.0 * 180.0))

rotation :: Vector3d -> [Vector3d]
rotation (Vector3d x y z) = [Vector3d 1.0 (-z) y, Vector3d z 1.0 (-x), Vector3d (-y) x 1.0]
