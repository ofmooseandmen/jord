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
    ( Params(..)
    , Rates(..)
    , StaticTf(..)
    , DynamicTf(..)
    , sTransform
    , dTransform
    , transformPos
    , transformPosAt
    ) where

import Data.Geo.Jord.Model
import Data.Geo.Jord.Position
import Data.Geo.Jord.Vector3d

-- | 7-parameter transformation parameters (Helmert).
data Params = Params
    { translation :: Vector3d -- ^ translation vector containing the three translations along the coordinate axes: tx, ty, tz in __millimetres__.
    , scaleFactor :: Double -- ^  scale factor (unitless) in __part per billion__.
    , rotation :: Vector3d -- ^  rotation matrix (orthogonal) consisting of the three axes rx, ry, rz in __millidegrees__.
    }

-- | Transformation rates: used when for the 14-parameter transformation (Helmert).
data Rates = Rates
    { translationRate :: Vector3d -- ^ translation rate in __millimetres per year__.
    , scaleFactorRate :: Double -- ^ scale factor rate in __part per billion per year__.
    , rotationRate :: Vector3d -- ^ rotation rate in __millidegrees per year__.
    }

-- | Static transformation between 2 ellispoidal models.
data StaticTf = StaticTf
    { fromModel :: ModelId -- ^ from model.
    , toModel :: ModelId -- ^ to model.
    , params :: Params -- ^ parameters.
    }

-- | Dynamic transformation between 2 ellispoidal models: static part, epoch (t0 of the rates) and rates.
data DynamicTf =
    DynamicTf StaticTf
              Epoch
              Rates

sTransform :: (Ellipsoidal a, Ellipsoidal b) => a -> b -> [StaticTf] -> [StaticTf]
sTransform _ _ _ = []

dTransform :: (EllipsoidalT0 a, EllipsoidalT0 b) => a -> b -> [DynamicTf] -> [DynamicTf]
dTransform _ _ _ = []

transformPos ::
       (Ellipsoidal a, Ellipsoidal b) => Position a -> b -> [StaticTf] -> Maybe (Position b)
transformPos _ _ _ = Nothing

transformPosAt ::
       (EllipsoidalT0 a, EllipsoidalT0 b)
    => Position a
    -> Epoch
    -> b
    -> [DynamicTf]
    -> Maybe (Position b)
transformPosAt _ _ _ _ = Nothing
