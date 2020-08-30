module Data.Geo.Jord.Conversion
    ( toGeodetic
    , toGeocentric
    , nvectorToGeocentric
    , nvectorFromGeocentric
    ) where

import Data.Geo.Jord.Ellipsoid (Ellipsoid, eccentricity, equatorialRadius, isSphere, polarRadius)
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length
import Data.Geo.Jord.Math3d (V3(..))
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Model, surface)

toGeodetic :: (Model m) => Geocentric.Position m -> Geodetic.Position m
toGeodetic p = Geodetic.nvectorHeightPos' nv h (Geocentric.model p)
  where
    (nv, h) = nvectorFromGeocentric (Geocentric.coords p) (surface . Geocentric.model $ p)

toGeocentric :: (Model m) => Geodetic.Position m -> Geocentric.Position m
toGeocentric p = Geocentric.Position v (Geodetic.model p)
  where
    v = nvectorToGeocentric (Geodetic.nvector p, Geodetic.height p) (surface . Geodetic.model $ p)

-- | @nvectorToGeocentric (nv, h) e@ returns the geocentric coordinates equivalent to the given
-- /n/-vector @nv@ and height @h@ using the ellispoid @e@.
nvectorToGeocentric :: (V3, Length) -> Ellipsoid -> Geocentric.Coordinates
nvectorToGeocentric (nv, h) e
    | isSphere e = nvectorToGeocentricS (nv, h) (equatorialRadius e)
    | otherwise = nvectorToGeocentricE (nv, h) e

nvectorToGeocentricS :: (V3, Length) -> Length -> Geocentric.Coordinates
nvectorToGeocentricS (nv, h) r = Geocentric.metres cx cy cz
  where
    n = Length.add h r
    (V3 cx cy cz) = Math3d.scale nv (Length.toMetres n)

nvectorToGeocentricE :: (V3, Length) -> Ellipsoid -> Geocentric.Coordinates
nvectorToGeocentricE (nv, h) e = Geocentric.metres cx cy cz
  where
    a = Length.toMetres . equatorialRadius $ e
    b = Length.toMetres . polarRadius $ e
    nx = vx nv
    ny = vy nv
    nz = vz nv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx * nx * m) + (ny * ny * m) + (nz * nz))
    h' = Length.toMetres h
    cx = n * m * nx + h' * nx
    cy = n * m * ny + h' * ny
    cz = n * nz + h' * nz

-- | @nvectorFromGeocentric g e@ returns the /n/-vector equivalent to the geocentric
-- coordinates @g@ using the ellispoid @e@.
nvectorFromGeocentric :: Geocentric.Coordinates -> Ellipsoid -> (V3, Length)
nvectorFromGeocentric g e
    | isSphere e = nvectorFromGeocentricS g (equatorialRadius e)
    | otherwise = nvectorFromGeocentricE g e

nvectorFromGeocentricS :: Geocentric.Coordinates -> Length -> (V3, Length)
nvectorFromGeocentricS g r = (Math3d.unit gm, h)
  where
    gm = Geocentric.toMetres g
    h = Length.subtract (Length.metres (Math3d.norm gm)) r

nvectorFromGeocentricE :: Geocentric.Coordinates -> Ellipsoid -> (V3, Length)
nvectorFromGeocentricE g e = (nvecEllipsoidal d e2 k px py pz, Length.metres h)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = Length.toMetres . equatorialRadius $ e
    a2 = a * a
    gm = Geocentric.toMetres g
    px = vx gm
    py = vy gm
    pz = vz gm
    p = (px * px + py * py) / a2
    q = ((1 - e2) / a2) * (pz * pz)
    r = (p + q - e4) / 6.0
    s = (e4 * p * q) / (4.0 * r * r * r)
    t = (1.0 + s + sqrt (s * (2.0 + s))) ** (1 / 3)
    u = r * (1.0 + t + 1.0 / t)
    v = sqrt (u * u + q * e4)
    w = e2 * (u + v - q) / (2.0 * v)
    k = sqrt (u + v + w * w) - w
    d = k * sqrt (px * px + py * py) / (k + e2)
    h = ((k + e2 - 1.0) / k) * sqrt (d * d + pz * pz)

nvecEllipsoidal :: Double -> Double -> Double -> Double -> Double -> Double -> V3
nvecEllipsoidal d e2 k px py pz = V3 nx ny nz
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx = s * a * px
    ny = s * a * py
    nz = s * pz
