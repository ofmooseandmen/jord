-- |
-- Module:      Data.Geo.Jord.Position
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for converting positions between coordinates systems.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Position
    -- * The 'HorizontalPosition' type
    ( HorizontalPosition(..)
    -- * The 'Geodetic3D' type
    , GeographicPosition(..)
    -- Horizontal & Vertical Positions - 'GeographicPosition'
    , EcefPosition(ex, ey, ez)
    , GeodeticPosition(getLatLong)
    , NVectorPosition(getNVector)
    -- | Smart constructors
    , ecefPosition
    , geodeticPosition
    , nvectorPosition
    -- | Remarkable positions
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector

-- | Horizontal position.
class (Eq a) =>
      HorizontalPosition a
    -- | Converts a 'NVector' into 'HorizontalPosition' instance.
    where
    fromNVector :: NVector -> a
    -- | Converts a 'HorizontalPosition' into 'NVector' instance.
    toNVector :: a -> NVector

instance HorizontalPosition LatLong where
    fromNVector v = latLong lat lon
      where
        lat = atan2' (nz v) (sqrt (nx v * nx v + ny v * ny v))
        lon = atan2' (ny v) (nx v)
    toNVector g = nvector x' y' z'
      where
        lat = latitude g
        lon = longitude g
        cl = cos' lat
        x' = cl * cos' lon
        y' = cl * sin' lon
        z' = sin' lat

instance HorizontalPosition NVector where
    fromNVector v = v
    toNVector v = v

-- | Earth-centered, earth-fixed (ECEF) position.
--
-- Orientation: z-axis points to the North Pole along the Earth's rotation axis,
-- x-axis points towards the point where latitude = longitude = 0.
data EcefPosition = EcefPosition
    { ex :: Double
    , ey :: Double
    , ez :: Double
    , ee :: Ellipsoid
    } deriving (Eq, Show)

-- | 'EcefPosition' from given x, y, z and ellipsoid.
ecefPosition :: Double -> Double -> Double -> Ellipsoid -> EcefPosition
ecefPosition = EcefPosition

-- | Geodetic latitude, longitude and height.
data GeodeticPosition = GeodeticPosition
    { getLatLong :: LatLong
    , gpH :: Double -- TODO Height
    , gpE :: Ellipsoid
    } deriving (Eq)

instance Show GeodeticPosition where
    show (GeodeticPosition ll h e) = "lat/long = " ++ show ll ++ "; height = " ++ show h ++ "; ellipsoid = " ++ show e

-- | 'GeodeticPosition' from given 'LatLong', height and ellipsoid.
geodeticPosition :: LatLong -> Double -> Ellipsoid -> GeodeticPosition
geodeticPosition = GeodeticPosition

-- | 'NVector' and height.
data NVectorPosition = NVectorPosition
    { getNVector :: NVector
    , nvH :: Double -- TODO Height
    , nvE :: Ellipsoid
    } deriving (Eq)

instance Show NVectorPosition where
    show (NVectorPosition nv h e) = "n-vector = " ++ show nv ++ "; height = " ++ show h ++ "; ellipsoid = " ++ show e

-- | 'NVectorPosition' from given 'NVector', height  and ellipsoid.
nvectorPosition :: NVector -> Double -> Ellipsoid -> NVectorPosition
nvectorPosition = NVectorPosition

-- | Geographic position.
class (Eq a) =>
      GeographicPosition a
    -- | Converts a 'NVectorPosition' into 'GeographicPosition' instance.
    where
    fromNVectorPosition :: NVectorPosition -> a
    -- | Converts a 'GeographicPosition' into 'NVector' instance.
    toNVectorPosition :: a -> NVectorPosition
    -- | Vertical position.
    height :: a -> Double -- TODO: Height.hs
    -- Reference ellipsoid of this position
    ellipsoid :: a -> Ellipsoid
    -- | @fromEcefPosition ev@ transforms the geocentric Earth-Centered Earth-Fixed (ECEF)
    -- Cartesian position to a 'GeographicPosition'
    -- The position refers to the reference 'Ellipsoid' of @ev@.
    fromEcefPosition :: EcefPosition -> a
    -- | @toEcefPosition a@ transforms a 'GeographicPosition'
    -- to geocentric Earth-Centered Earth-Fixed (ECEF) Cartesian position using
    -- the reference 'Ellipsoid' of @a@.
    toEcefPosition :: a -> EcefPosition

instance GeographicPosition GeodeticPosition where
    fromNVectorPosition (NVectorPosition nv h e) = GeodeticPosition (fromNVector nv) h e
    toNVectorPosition (GeodeticPosition ll h e) = NVectorPosition (toNVector ll) h e
    height = gpH
    ellipsoid = gpE
    fromEcefPosition ep = GeodeticPosition (fromNVector nv' :: LatLong) h (ee ep)
      where
        (nv', h) = fromEcef ep
    toEcefPosition (GeodeticPosition ll' h e) = toEcef (toNVector ll') h e

instance GeographicPosition NVectorPosition where
    fromNVectorPosition nv = nv
    toNVectorPosition nv = nv
    height = nvH
    ellipsoid = nvE
    fromEcefPosition ep = NVectorPosition nv' h (ee ep)
      where
        (nv', h) = fromEcef ep
    toEcefPosition (NVectorPosition nv' h e) = toEcef nv' h e

instance GeographicPosition EcefPosition where
    fromNVectorPosition (NVectorPosition nv' h e) = toEcef nv' h e
    toNVectorPosition ep = NVectorPosition nv' h (ee ep)
      where
        (nv', h) = fromEcef ep
    height ep = snd (fromEcef ep)
    ellipsoid = ee
    fromEcefPosition ep = ep
    toEcefPosition ep = ep

toEcef :: NVector -> Double -> Ellipsoid -> EcefPosition
toEcef p h e = EcefPosition ex' ey' ez' e
  where
    nv = unit p
    a = toMetres (equatorialRadius e)
    b = toMetres (polarRadius e)
    nx' = nx nv
    ny' = ny nv
    nz' = nz nv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx' * nx' * m) + (ny' * ny' * m) + (nz' * nz'))
    ex' = n * m * nx' + h * nx'
    ey' = n * m * ny' + h * ny'
    ez' = n * nz' + h * nz'

fromEcef :: EcefPosition -> (NVector, Double)
fromEcef p'@(EcefPosition px py pz e) = (nvec d e2 k p', h)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = toMetres (equatorialRadius e)
    p = (px * px + py * py) / (a * a)
    q = ((1 - e2) / (a * a)) * (pz * pz)
    r = (p + q - e4) / 6.0
    s = (e4 * p * q) / (4.0 * r * r * r)
    t = (1.0 + s + sqrt (s * (2.0 + s))) ** (1 / 3)
    u = r * (1.0 + t + 1.0 / t)
    v = sqrt (u * u + q * e4)
    w = e2 * (u + v - q) / (2.0 * v)
    k = sqrt (u + v + w * w) - w
    d = k * sqrt (px * px + py * py) / (k + e2)
    h = ((k + e2 - 1.0) / k) * sqrt (d * d + pz * pz)

nvec :: Double -> Double -> Double -> EcefPosition -> NVector
nvec d e2 k (EcefPosition px py pz _) = nvector nx' ny' nz'
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx' = s * a * px
    ny' = s * a * py
    nz' = s * pz

-- | Horizontal position of the North Pole.
northPole :: (HorizontalPosition a) => a
northPole = fromNVector (nvector 0.0 0.0 1.0)

-- | Horizontal position of the South Pole.
southPole :: (HorizontalPosition a) => a
southPole = fromNVector (nvector 0.0 0.0 (-1.0))
