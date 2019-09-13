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
    ( Tx
    , modelA
    , modelB
    , txParams
    , inverseTxParams
    , StaticTx
    , staticTx
    , DynamicTx
    , dynamicTx
    , TxParams7
    , TxRates
    , TxParams15(..)
    , txParams7
    , txRates
    , transformCoords
    , transformCoordsAt
    ) where

import Data.Geo.Jord.Model
import Data.Geo.Jord.Position
import Data.Geo.Jord.Vector3d

-- | Coordinate transformation between 2 models (A & B).
data Tx a =
    Tx
        { modelA :: ModelId -- ^  model A.
        , modelB :: ModelId -- ^ model B.
        , txParams :: a -- ^ transformation parameters - i.e. 'modelA'-> 'modelB'
        , inverseTxParams :: a -- ^ parameters of inverse transformation - i.e. 'modelB' -> 'modelA'.
        }

-- | Time-__independent__ transformation between 2 ellispoidal models: 7-parameter transformation.
type StaticTx = Tx TxParams7

-- | 'StaticTx' from given direct transformation parameters.
staticTx :: ModelId -> ModelId -> TxParams7 -> Tx TxParams7
staticTx f t p = Tx f t p (inverseTxParams7 p)

-- | Time-__dependent__ transformation between 2 time-dependant ellispoidal models: 15-parameter transformation.
type DynamicTx = Tx TxParams15

-- | 'DynamicTx' from given direct transformation parameters.
dynamicTx :: ModelId -> ModelId -> TxParams15 -> Tx TxParams15
dynamicTx f t p = Tx f t p (inverseTxParams15 p)

-- | 7-parameter transformation (Helmert); use 'txParams7' to construct.
data TxParams7 =
    TxParams7 !Vector3d !Double !Vector3d

-- | Transformation rates for the 15-parameter transformation (Helmert); use 'txRates' to construct.
data TxRates =
    TxRates !Vector3d !Double !Vector3d

-- | Epoch and 14-parameter transformation (Helmert).
data TxParams15 =
    TxParams15 Epoch TxParams7 TxRates

-- | 7-parameter transformation (Helmert) from given translation vector, scale factor and rotation matrix.
txParams7 ::
       (Double, Double, Double) -- ^ translation vector containing the three translations along the coordinate axes: tx, ty, tz in __millimetres__
    -> Double -- ^ scale factor (unitless) expressed in __parts per billion__
    -> (Double, Double, Double) -- ^ rotation matrix (orthogonal) consisting of the three axes rx, ry, rz in __milliarcseconds__
    -> TxParams7
txParams7 c s r = TxParams7 (mmToMetres c) (s / 1e9) (masToRadians r)

-- | @inverseTxParams7 t@ returns the 7-parameters of the inverse of transformation @t@.
inverseTxParams7 :: TxParams7 -> TxParams7
inverseTxParams7 (TxParams7 c s r) = TxParams7 ic is ir
  where
    ic = vscale c (-1.0)
    is = (-s)
    ir = vscale r (-1.0)

-- | rates of the 15-parameter translation (Helmert) from given translation rates, scale factor rate and rotation rates.
txRates ::
       (Double, Double, Double) -- ^ translation rate in __millimetres per year__.
    -> Double -- ^ scale factor rate in __part per billion per year__.
    -> (Double, Double, Double) -- ^ rotation rate in __milliarcseconds per year__.
    -> TxRates
txRates c s r = TxRates (mmToMetres c) (s / 1e9) (masToRadians r)

-- | @inverseTxParams15 t@ returns the 15-parameters of the inverse of transformation @t@.
inverseTxParams15 :: TxParams15 -> TxParams15
inverseTxParams15 (TxParams15 e p (TxRates c s r)) =
    TxParams15 e (inverseTxParams7 p) (TxRates ic is ir)
  where
    ic = vscale c (-1.0)
    is = (-s)
    ir = vscale r (-1.0)

-- | @transformCoords p1 m2 tx@ transforms the coordinates of the position @p1@ from its coordinate system
-- into the coordinate system defined by the model @m2@ using the 7-parameters transformation @tx@.
--
-- ==== __Examples__
--
-- >>> let tx = txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599) -- WGS84 -> NAD83
-- >>> let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
-- >>> pNAD83 = transformCoords pWGS84 NAD83 tx
-- >>> 48°41'31.523"N,6°11'3.723"E 188.1212m (NAD83)
--
transformCoords :: (Ellipsoidal a, Ellipsoidal b) => Position a -> b -> TxParams7 -> Position b
transformCoords p1 m2 (TxParams7 c s r) = transformCoords' p1 m2 c s r

-- | @transformCoordsAt p1 e m2 tx@ transforms the coordinates of the position @p1@ observed at epoch @e@
-- from its coordinate system into the coordinate system defined by the model @m2@ using
-- the 15-parameters transformation @tx@.
--
-- ==== __Examples__
--
-- >>> let tx7 = txParams7 (53.7, 51.2, -55.1) 1.2 (0.891, 5.39, -8.712)
-- >>> let txR = txRates (0.1, 0.1, -1.9) 0.11 (0.81, 0.49, -0.792)
-- >>> let tx = TxParams15 (Epoch 2000.0) tx7 txR -- ITRF2014 -> ETRF2000
-- >>> let pITRF2014 = latLongHeightPos 48.6921 6.1844 (metres 188) ITRF2014
-- >>> pETRF2000 = transformCoordsAt pITRF2014 (Epoch 2019.0) ETRF2000 tx
-- >>> 48°41'31.561"N,6°11'3.865"E 188.0178m (ETRF2000)
--
transformCoordsAt ::
       (EllipsoidalT0 a, EllipsoidalT0 b) => Position a -> Epoch -> b -> TxParams15 -> Position b
transformCoordsAt p1 (Epoch e) m2 (TxParams15 (Epoch pe) (TxParams7 c s r) (TxRates rc rs rr)) =
    transformCoords' p1 m2 c' s' r'
  where
    de = pe - e
    c' = vadd c (vscale rc de)
    s' = s + de * rs
    r' = vadd r (vscale rr de)

transformCoords' ::
       (Model a, Model b) => Position a -> b -> Vector3d -> Double -> Vector3d -> Position b
transformCoords' p1 m2 c s r = geocentricMetresPos v2x v2y v2z m2
  where
    v1 = gcvec p1
    (Vector3d v2x v2y v2z) = vadd c (vscale (vmultm v1 (rotation r)) (1.0 + s))

mmToMetres :: (Double, Double, Double) -> Vector3d
mmToMetres (cx, cy, cz) = vscale (Vector3d cx cy cz) (1.0 / 1000.0)

masToRadians :: (Double, Double, Double) -> Vector3d
masToRadians (rx, ry, rz) = vscale (Vector3d rx ry rz) (pi / (3600.0 * 1000.0 * 180.0))

rotation :: Vector3d -> [Vector3d]
rotation (Vector3d x y z) = [Vector3d 1.0 (-z) y, Vector3d z 1.0 (-x), Vector3d (-y) x 1.0]
