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
    (
    -- * The 'Geodetic2D' type
      Geodetic2D(..)
    -- * The 'Geodetic3D' type
    , Geodetic3D(..)
    -- Horizontal & Vertical Positions - 'Geodetic3D'
    , EcefVector(ex, ey, ez)
    , LatLongH(getLatLong)
    , NVectorH(getNVector)
    -- | Smart constructors
    , ecefVector
    , latLongHeight
    , nvectorHeight
    -- | Remarkable positions
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector

-- | Horizontal geodetic position.
class (Eq a) => Geodetic2D a where
    -- | Converts a 'NVector' into 'Geodetic2D' instance.
    fromNVector :: NVector -> a
    -- | Converts a 'Geodetic2D' into 'NVector' instance.
    toNVector :: a -> NVector

instance Geodetic2D LatLong where
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

instance Geodetic2D NVector where
    fromNVector v = v
    toNVector v = v

-- | Earth-centered, earth-fixed (ECEF) position.
--
-- Orientation: z-axis points to the North Pole along the Earth's rotation axis,
-- x-axis points towards the point where latitude = longitude = 0.
data EcefVector = EcefVector
    { ex :: Double
    , ey :: Double
    , ez :: Double
    } deriving (Eq, Show)

-- | 'EcefVector' from given x, y, z.
ecefVector :: Double -> Double -> Double -> EcefVector
ecefVector = EcefVector

-- | Geodetic latitude, longitude and height.
data LatLongH = LatLongH
    { getLatLong :: LatLong
    , llH :: Double -- TODO Height
    } deriving (Eq)

instance Show LatLongH where
    show (LatLongH ll h) = "lat/long = " ++ show ll ++ "; height = " ++ show h

-- | 'LatLongH' from given 'LatLong' and height.
latLongHeight :: LatLong -> Double -> LatLongH
latLongHeight = LatLongH

-- | 'NVector' and height.
data NVectorH = NVectorH
    { getNVector :: NVector
    , nvH :: Double -- TODO Height
    } deriving (Eq)

instance Show NVectorH where
    show (NVectorH nv h) = "n-vector = " ++ show nv ++ "; height = " ++ show h

-- | 'NVectorH' from given 'NVector' and height.
nvectorHeight :: NVector -> Double -> NVectorH
nvectorHeight = NVectorH

-- | Horizontal geodetic position and height.
class (Eq a) => Geodetic3D a where
    -- | Converts a 'NVector' into 'Geodetic3D' instance.
    fromNVectorH :: NVectorH -> a
    -- | Converts a 'Geodetic3D' into 'NVector' instance.
    toNVectorH :: a -> NVectorH
    -- | Vertical position.
    height :: a -> Double -- TODO: Height.hs and Length -> Distance
    -- | @geodeticToEcef a e@ transforms the geodetic 3D position
    -- to geocentric Earth-Centered Earth-Fixed (ECEF) Cartesian position.
    -- The geodetic position refers to the reference 'Ellipsoid' @e@.
    geodeticToEcef :: a -> Ellipsoid -> EcefVector
    -- | @ecefToGeodetic ev e@ transforms the geocentric Earth-Centered Earth-Fixed (ECEF)
    -- Cartesian position represented to a geodetic 3D position.
    -- The geodetic position refers to the reference 'Ellipsoid' @e@.
    ecefToGeodetic :: EcefVector -> Ellipsoid -> a

instance Geodetic3D LatLongH where
    fromNVectorH nv = LatLongH (fromNVector (getNVector nv)) (height nv)
    toNVectorH llh = NVectorH (toNVector (getLatLong llh)) (height llh)
    height = llH
    geodeticToEcef (LatLongH ll' h) = geodeticToEcef' (toNVector ll') h
    ecefToGeodetic ev e = LatLongH (fromNVector nv' :: LatLong) h
      where
        (nv', h) = ecefToGeodetic' ev e

instance Geodetic3D NVectorH where
    fromNVectorH nv = nv
    toNVectorH nv = nv
    height = nvH
    geodeticToEcef (NVectorH nv' h) = geodeticToEcef' nv' h
    ecefToGeodetic ev e = NVectorH nv' h
      where
        (nv', h) = ecefToGeodetic' ev e

geodeticToEcef' :: NVector -> Double -> Ellipsoid -> EcefVector
geodeticToEcef' p h e = EcefVector ex' ey' ez'
  where
    nv = unit p
    a = toMetres (equatorialRadius e)
    b = toMetres (polarRadius e)
    nx' = nx nv
    ny' = ny nv
    nz' = nz nv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx' * nx') * m + (ny' * ny' * m) + (nz' * nz'))
    ex' = n * m * nx' + h * nx'
    ey' = n * m * ny' + h * ny'
    ez' = n * nz' + h * nz'

ecefToGeodetic' :: EcefVector -> Ellipsoid -> (NVector, Double)
ecefToGeodetic' p'@(EcefVector px py pz) e = (nvec d e2 k p', h)
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

nvec :: Double -> Double -> Double -> EcefVector -> NVector
nvec d e2 k (EcefVector px py pz) = nvector nx' ny' nz'
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx' = s * a * px
    ny' = s * a * py
    nz' = s * pz

-- | Horizontal position of the North Pole.
northPole :: (Geodetic2D a) => a
northPole = fromNVector (nvector 0.0 0.0 1.0)

-- | Horizontal position of the South Pole.
southPole :: (Geodetic2D a) => a
southPole = fromNVector (nvector 0.0 0.0 (-1.0))
