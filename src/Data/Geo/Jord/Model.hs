module Data.Geo.Jord.Model
    ( LongitudeRange(..)
    , ModelName(..)
    , Model(..)
    , Spherical
    , Ellipsoidal
    , StaticSurface
    , Epoch(..)
    , DynamicSurface(..)
    ) where

import Data.Geo.Jord.Ellipsoid

-- | longitude range.
data LongitudeRange
    = L180 -- ^  [-180°, 180°]: range for Earth, Moon and Sun.
    | L360 -- ^  [0°, 360°]: range for other celestial bodies (e.g. Mars).

newtype ModelName =
    ModelName String
    deriving (Eq)

instance Show ModelName where
    show (ModelName n) = n

class (Eq a, Show a) =>
      Model a
    where
    surface :: a -> Ellipsoid
    longitudeRange :: a -> LongitudeRange
    name :: a -> ModelName

class (Model a) =>
      Spherical a


class (Model a) =>
      Ellipsoidal a


class (Ellipsoidal a) =>
      StaticSurface a


data Epoch =
    Epoch
        { year :: Int
        , decimal :: Int
        }
    deriving (Eq, Show)

class (Ellipsoidal a) =>
      DynamicSurface a
    where
    epoch :: a -> Epoch
{-|
data WGS84 =
    WGS84

instance Datum WGS84 where
    surface _ = eWGS84
    longitudeRange _ = L180
    name _ = DatumName "WGS84"

instance Ellipsoidal WGS84 where
    epoch _ = Epoch 1984 0

instance Eq WGS84 where
    _ == _ = True

instance Show WGS84 where
    show = show . name

data GRS80 =
    GRS80

instance Datum GRS80 where
    surface _ = eGRS80
    longitudeRange _ = L180
    name _ = DatumName "GRS80"

instance Ellipsoidal GRS80 where
    epoch _ = Epoch 1980 0

instance Eq GRS80 where
    _ == _ = True

instance Show GRS80 where
    show = show . name

data S84 =
    S84

instance Datum S84 where
    surface _ = toSphere eWGS84
    longitudeRange _ = L180
    name _ = (DatumName "Mean Radius WGS84")

instance Spherical S84

instance Eq S84 where
    _ == _ = True

instance Show S84 where
    show = show . name
-}