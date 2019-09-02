-- |
-- Module:      Data.Geo.Jord.Bodies
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Ellipsoidal and derived spherical models representing celestial bodies.
--
-- TODO: CelestialBody.hs and generated file CelestialBodies.hs
module Data.Geo.Jord.Bodies
    ( Shape(..)
    , Ellipse(..)
    , LongitudeRange(..)
    , Model(..)
    , Spherical
    , Ellipsoidal
    , modelRadius
    , modelEccentricity
    , modelPolarRadius
    , meanRadius
    , eccentricity
    , polarRadius
    , flattening
    -- * models.
    , WGS84(..)
    , S84(..)
    , GRS80(..)
    , S80(..)
    , WGS72(..)
    , S72(..)
    , Moon(..)
    , Mars(..)
    , SMars(..)
    ) where

import Data.Geo.Jord.Length

-- | A shape representing a celestial body (e.g. the Earth).
data Shape
    = Ellipsoid Ellipse -- ^ ellipsoid.
    | Sphere Length -- ^ sphere.
    deriving (Eq, Show)

-- | Primary ellipsoid parameters.
data Ellipse = Ellipse
    { equatorialRadius :: Length -- ^ equatorial radius or semi-major axis (a).
    , inverseFlattening :: Double -- ^ inverse flattening.
    } deriving (Eq, Show)

-- | longitude range.
data LongitudeRange
    = L180 -- ^  [-180°, 180°]: range for Earth, Moon and Sun
    | L360 -- ^  [0°, 360°]: range for other celestial bodies (e.g. Mars)

-- | Model representing a celestial body (e.g. the Earth).
--
-- A model provides the shape of the celestial body (an ellispoid or a sphere) and
-- the longitude range (defaults to [-180°, 180°]).
class (Eq a, Show a) =>
      Model a
    where
    shape :: a -> Shape -- ^ the shape representing the celestial body.
    longitudeRange :: a -> LongitudeRange -- ^ longitude range (defaults to [-180°, 180°]).
    longitudeRange _ = L180

-- | Spherical model, use this when approximating the shape of a celestial body is acceptable
-- for the calculations to be performed; it gives access to more functions (such as kinematics
-- and path intersections).
class (Model a) =>
      Spherical a


-- | Ellipsoidal model.
class (Model a) =>
      Ellipsoidal a


-- | Radius of the given model.
-- For an ellispoidal model it is the 'meanRadius'.
modelRadius :: (Model a) => a -> Length
modelRadius m =
    case shape m of
        (Sphere r) -> r
        (Ellipsoid e) -> meanRadius e

-- | Eccentricity of the given model.
-- For a spherical model it is @0@.
modelEccentricity :: (Model a) => a -> Double
modelEccentricity m =
    case shape m of
        (Sphere _) -> 0
        (Ellipsoid e) -> eccentricity e

-- | Polar radius of the given model.
-- For a spherical model it is the radius of the sphere.
modelPolarRadius :: (Model a) => a -> Length
modelPolarRadius m =
    case shape m of
        (Sphere r) -> r
        (Ellipsoid e) -> polarRadius e

-- | Eccentricity of the given ellipse
eccentricity :: Ellipse -> Double
eccentricity e = sqrt (1.0 - (b * b) / (a * a))
  where
    a = semiMajorAxis e
    b = semiMinorAxis a (flattening e)

-- | Polar radius or semi-minor axis (b) of the given ellipse.
polarRadius :: Ellipse -> Length
polarRadius e = metres (semiMinorAxis a f)
  where
    a = semiMajorAxis e
    f = flattening e

-- | Flattening of the given ellipse.
flattening :: Ellipse -> Double
flattening e = 1.0 / inverseFlattening e

-- | Mean radius of the given ellipse.
meanRadius :: Ellipse -> Length
meanRadius e = metres ((2.0 * a + b) / 3.0)
  where
    a = semiMajorAxis e
    b = semiMinorAxis a (flattening e)

-- | World Geodetic System WGS84 ellipsoid.
data WGS84 =
    WGS84

instance Model WGS84 where
    shape _ = Ellipsoid (Ellipse (metres 6378137.0) 298.257223563)

instance Ellipsoidal WGS84

instance Eq WGS84 where
    _ == _ = True

instance Show WGS84 where
    show _ = "WGS84"

-- Spherical earth model derived from 'WGS84'.
data S84 =
    S84

instance Model S84 where
    shape _ = Sphere (modelRadius WGS84)

instance Spherical S84

instance Eq S84 where
    _ == _ = True

instance Show S84 where
    show _ = "S84"

-- | Geodetic Reference System 1980 ellipsoid.
data GRS80 =
    GRS80

instance Model GRS80 where
    shape _ = Ellipsoid (Ellipse (metres 6378137.0) 298.257222101)

instance Ellipsoidal GRS80

instance Eq GRS80 where
    _ == _ = True

instance Show GRS80 where
    show _ = "GRS80"

-- Spherical earth model derived from 'GRS80'.
data S80 =
    S80

instance Model S80 where
    shape _ = Sphere (modelRadius GRS80)

instance Spherical S80

instance Eq S80 where
    _ == _ = True

instance Show S80 where
    show _ = "S80"

-- | World Geodetic System WGS72 ellipsoid.
data WGS72 =
    WGS72

instance Model WGS72 where
    shape _ = Ellipsoid (Ellipse (metres 6378135.0) 298.26)

instance Ellipsoidal WGS72

instance Eq WGS72 where
    _ == _ = True

instance Show WGS72 where
    show _ = "WGS72"

-- Spherical earth model derived from 'WGS72'.
data S72 =
    S72

instance Model S72 where
    shape _ = Sphere (modelRadius WGS72)

instance Spherical S72

instance Eq S72 where
    _ == _ = True

instance Show S72 where
    show _ = "S72"

-- | Moon model (Moon is nearly a perfect sphere).
data Moon =
    Moon

instance Model Moon where
    shape _ = Sphere (kilometres 1737.4)

instance Spherical Moon

instance Eq Moon where
    _ == _ = True

instance Show Moon where
    show _ = "Moon"

-- | Mars ellipsoidal model (from the Mars Orbiter Laser Altimeter).
data Mars =
    Mars

instance Model Mars where
    shape _ = Ellipsoid (Ellipse (metres 3398627.0) 169.8)
    longitudeRange _ = L360

instance Ellipsoidal Mars

instance Eq Mars where
    _ == _ = True

instance Show Mars where
    show _ = "Mars"

-- Mars spherical model - derived from 'Mars'.
data SMars =
    SMars

instance Model SMars where
    shape _ = Sphere (modelRadius Mars)
    longitudeRange _ = L360

instance Spherical SMars

instance Eq SMars where
    _ == _ = True

instance Show SMars where
    show _ = "Mars (Sphere)"

-- | semi-major axis (a) in metres.
semiMajorAxis :: Ellipse -> Double
semiMajorAxis = toMetres . equatorialRadius

-- | Computes the polar semi-minor axis (b) from semi major axis @a@ and flattening @f@.
semiMinorAxis :: Double -> Double -> Double
semiMinorAxis a f = a * (1.0 - f)
