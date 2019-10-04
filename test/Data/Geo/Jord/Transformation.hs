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
-- In order to use this module you should start with the following imports:
--
-- @
--     import Data.Geo.Jord.Position
--     import Data.Geo.Jord.Transformation
-- @
--
--
module Data.Geo.Jord.Transformation
    ( transformCoords
    , transformCoords'
    , transformCoordsAt
    , transformCoordsAt'
    -- * re-exported for convenience
    , module Data.Geo.Jord.Tx
    , module Data.Geo.Jord.Txs
    ) where

import Data.Geo.Jord.Position
import Data.Geo.Jord.Tx
import Data.Geo.Jord.Txs

-- | @transformCoords p1 m2 g@ transforms the coordinates of the position @p1@ from its coordinate
-- system into the coordinate system defined by the model @m2@ using the graph @g@ to find the
-- sequence of transformation parameters. Returns 'Nothing' if the given graph does not contain a
-- transformation from @m1@ to @m2@ - see 'txParamsBetween'.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Position
-- >>> import Data.Geo.Jord.Transformation
-- >>>
-- >>> let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
-- >>> transformCoords pWGS84 NAD83 staticTxs
-- Just 48°41'31.523"N,6°11'3.723"E 188.1212m (NAD83)
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
-- >>> import Data.Geo.Jord.Position
-- >>> import Data.Geo.Jord.Transformation
-- >>>
-- >>> let tx = txParams7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599) -- WGS84 -> NAD83
-- >>> let pWGS84 = wgs84Pos 48.6921 6.1844 (metres 188)
-- >>> transformCoords' pWGS84 NAD83 tx
-- 48°41'31.523"N,6°11'3.723"E 188.1212m (NAD83)
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
-- >>> import Data.Geo.Jord.Position
-- >>> import Data.Geo.Jord.Transformation
-- >>>
-- >>> let pITRF2014 = latLongHeightPos 48.6921 6.1844 (metres 188) ITRF2014
-- >>> transformCoordsAt pITRF2014 (Epoch 2019.0) NAD83_CORS96 dynamicTxs -- through ITRF2000
-- Just 48°41'31.538"N,6°11'3.722"E 188.112035m (NAD83_CORS96)
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
-- >>> import Data.Geo.Jord.Position
-- >>> import Data.Geo.Jord.Transformation
-- >>>
-- >>> let tx7 = txParams7 (53.7, 51.2, -55.1) 1.2 (0.891, 5.39, -8.712)
-- >>> let txR = txRates (0.1, 0.1, -1.9) 0.11 (0.81, 0.49, -0.792)
-- >>> let tx = TxParams15 (Epoch 2000.0) tx7 txR -- ITRF2014 -> ETRF2000
-- >>> let pITRF2014 = latLongHeightPos 48.6921 6.1844 (metres 188) ITRF2014
-- >>> transformCoordsAt' pITRF2014 (Epoch 2019.0) ETRF2000 tx
-- 48°41'31.561"N,6°11'3.865"E 188.0178m (ETRF2000)
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
