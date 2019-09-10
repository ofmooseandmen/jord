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
    ( TxParams7
    , TxRates
    , TxParams15(..)
    , StaticTx(..)
    , DynamicTx(..)
    , txParams7
    , inverseTxParams7
    , txRates
    , staticTx
    , dynamicTx
    , transformCoords
    , transformCoordsAt
    ) where

import Data.Geo.Jord.Model
import Data.Geo.Jord.Position
import Data.Geo.Jord.Vector3d

-- | 7-parameter transformation (Helmert); use 'txParams7' to construct.
data TxParams7 =
    TxParams7 !Vector3d !Double ![Vector3d]

-- | Transformation rates for the 14-parameter transformation (Helmert); use 'txRates' to construct.
data TxRates =
    TxRates Vector3d Double Vector3d

-- | Epoch and 14-parameter transformation (Helmert).
data TxParams15 =
    TxParams15 Epoch TxParams7 TxRates

-- | Time-__independent__ transformation between 2 ellispoidal models: id of from model, id of to model
-- and 7-parameter transformation.
data StaticTx =
    StaticTx ModelId ModelId TxParams7

-- | Time-__dependent__ transformation between 2 time-dependant ellispoidal models: id of from model, id of to model
-- and 15-parameter transformation.
data DynamicTx =
    DynamicTx ModelId ModelId TxParams15

-- \ 7-parameter transformation (Helmert) from given translation vector, scale factor and rotation matrix.
txParams7 ::
       (Double, Double, Double) -- ^ translation vector containing the three translations along the coordinate axes: tx, ty, tz in __millimetres__
    -> Double -- ^ scale factor (unitless) expressed in __parts per billion__
    -> (Double, Double, Double) -- ^ rotation matrix (orthogonal) consisting of the three axes rx, ry, rz in __milliarcseconds__
    -> TxParams7
txParams7 (cx, cy, cz) s (rx, ry, rz) = TxParams7 (vscale cv (1.0 / 1000.0)) (1.0 + s / 1e9) m
  where
    cv = Vector3d cx cy cz
    rv = Vector3d rx ry rz
    (Vector3d rx' ry' rz') = vscale rv (pi / (3600.0 * 1000.0 * 180.0))
    m = [Vector3d 1.0 (-rz') ry', Vector3d rz' 1.0 (-rx'), Vector3d (-ry') rx' 1.0]

-- | Inverse transformation parameters.
inverseTxParams7 :: TxParams7 -> TxParams7
inverseTxParams7 (TxParams7 c s r) = TxParams7 ic is ir
  where
    ic = vscale c (-1.0)
    is = 2.0 - s
    (rx, ry, rz) = (-vy (r !! 2), -vz (head r), -vx (r !! 1))
    ir = [Vector3d 1.0 (-rz) ry, Vector3d rz 1.0 (-rx), Vector3d (-ry) rx 1.0]

txRates :: Vector3d -> Double -> Vector3d -> TxRates
txRates _ _ _ = undefined

staticTx :: (Ellipsoidal a, Ellipsoidal b) => a -> b -> [StaticTx] -> [TxParams7]
staticTx _ _ _ = []

dynamicTx :: (EllipsoidalT0 a, EllipsoidalT0 b) => a -> b -> [DynamicTx] -> [TxParams15]
dynamicTx _ _ _ = []

-- | @transformCoords p1 m2 tx@ transforms the coordinates of the position @p1@ from its coordinate system
-- into the coordinate system defined by the model @m2@ and using the 7-parameters transformation @tx@.
--
-- ==== __Examples__
--
-- >>> let tx7 = txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599) -- WGS84 -> NAD83
-- >>> let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
-- >>> pNAD83 = transformCoords pWGS84 NAD83 tx7
-- >>> 48°41'31.523"N,6°11'3.723"E 188.1212m (NAD83)
--
transformCoords :: (Ellipsoidal a, Ellipsoidal b) => Position a -> b -> TxParams7 -> Position b
transformCoords p1 m2 (TxParams7 c s r) = geocentricMetresPos v2x v2y v2z m2
  where
    v1 = gcvec p1
    (Vector3d v2x v2y v2z) = vadd c (vscale (vmultm v1 r) s)

transformCoordsAt :: (EllipsoidalT0 a, EllipsoidalT0 b) => Position a -> Epoch -> b -> TxParams15 -> Position b
transformCoordsAt _ _ _ _ = undefined