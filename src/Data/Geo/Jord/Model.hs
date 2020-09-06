-- |
-- Module:      Data.Geo.Jord.Model
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Definition of celestial body models.
--
-- see "Data.Geo.Jord.Models" for supported models.
module Data.Geo.Jord.Model
    ( LongitudeRange(..)
    , ModelId(..)
    , Epoch(..)
    , Model(..)
    , Spherical
    , Ellipsoidal
    , EllipsoidalT0(..)
    ) where

import Data.Geo.Jord.Ellipsoid

-- | Longitude range.
data LongitudeRange
    = L180 -- ^  [-180°, 180°]: range for Earth, Moon and Sun.
    | L360 -- ^  [0°, 360°]: range for other celestial bodies (e.g. Mars).

-- | Epoch (decimal years) such as 2018.60: the 219th day of the year or August 7, 2018
-- in the Gregorian calendar.
newtype Epoch =
    Epoch Double
    deriving (Eq, Show)

-- | identifier of a model.
newtype ModelId =
    ModelId String
    deriving (Eq)

instance Show ModelId where
    show (ModelId i) = i

-- | Model for a celestial body: the same celestial body can be represented by different
-- models (e.g. Earth: WGS84, ITRF2014, Spherical, etc...).
class (Eq a, Show a) =>
      Model a
    where
    modelId :: a -> ModelId -- ^ model identifier, must be unique for coordinate transformation.
    surface :: a -> Ellipsoid -- ^ surface of the celestial body.
    longitudeRange :: a -> LongitudeRange -- ^ longitude range.

-- | Models that approximate the surface of the celestial body to a sphere.
-- Such an approximation is satisfactory for many purposes and allows a wide
-- range of calculations: see "Data.Geo.Jord.Kinematics", "Data.Geo.Jord.GreatCircle" and "Data.Geo.Jord.LocalFrames".
class (Model a) =>
      Spherical a


-- | Models that represent the surface of the celestial body with an ellispoid.
-- Compare to 'Spherical' models, less calculations are available and they are more CPU
-- intensive: see "Data.Geo.Jord.Geodesic" and "Data.Geo.Jord.LocalFrames", however those
-- calculations are more \"correct\".
-- Supports coordinates transformation between different ellispoidal models using 7-parameter
-- transformation (Helmert): see "Data.Geo.Jord.Transformation".
class (Model a) =>
      Ellipsoidal a


-- | Time-dependent 'Ellipsoidal' models, such as International Terrestrial Reference Frames (ITRF).
-- The epoch allows to account for unmodelled measurement biases and tectonic processes: supports
-- coordinates transformation between different time-dependent ellispoidal models at given epoch using
-- 15-parameter transformation (Helmert): see "Data.Geo.Jord.Transformation".
class (Ellipsoidal a) =>
      EllipsoidalT0 a
    where
    epoch :: a -> Epoch -- ^ epoch to which coordinates are referenced.
