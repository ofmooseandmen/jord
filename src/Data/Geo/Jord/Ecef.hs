-- |
-- Module:      Data.Geo.Jord.Ecef
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with earth-centered, earth-fixed (ECEF) positions.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Ecef
    ( geodeticToEcef
    , ecefToGeodetic
    ) where

-- TODO: rename Position, indicate that this module assumes ellipsoidal earth
-- TODO: describe orientation of NVector in HorizontalPosition
-- TODO: indicate that all latitudes are geodetic latitudes.

import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.HorizontalPosition
import Data.Geo.Jord.Length
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Vector3d

-- | @geodeticToEcef (p, h) e@ transforms the tuple geodetic position @p@ and ellipsoidal height @h@
-- to geocentric Earth-Centered Earth-Fixed (ECEF) Cartesian position.
-- The geodetic position refers to the reference 'Ellipsoid' @e@.
geodeticToEcef :: (HorizontalPosition a) => (a, Double) -> Ellipsoid -> Vector3d
geodeticToEcef (p, h) e = Vector3d ex ey ez
  where
    nv = unit (toNVector p)
    a = toMetres (equatorialRadius e)
    b = toMetres (polarRadius e)
    nx = x nv
    ny = y nv
    nz = z nv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx * nx) * m + (ny * ny * m) + (nz * nz))
    ex = n * m *  nx + h * nx
    ey = n * m * ny + h * ny
    ez = n * nz + h * nz

-- | @ecefToGeodetic v e@ transforms the geocentric Earth-Centered Earth-Fixed (ECEF)
-- Cartesian position represented by the 'Vector3d' @v@ to a tuple geodetic position
-- and ellipsoidal height. The geodetic v refers to the reference 'Ellipsoid' @e@.
ecefToGeodetic :: (HorizontalPosition a) => Vector3d -> Ellipsoid -> (a, Double)
ecefToGeodetic p'@(Vector3d px py pz) e = (fromNVector (nvec d e2 k p'), h)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = toMetres (equatorialRadius e)
    q = ((1 - e2) / (a * a)) * (pz * pz)
    p = (px * px + py * py) / (a * a)
    r = (p + q - e4) / 6.0
    s = (e4 * p * q) / (4 * r * r * r)
    t = (1.0 + s + sqrt (s * (2.0 + s))) ** (1 / 3)
    u = r * (1.0 + t + 1.0 / t)
    v = sqrt (u * u + q * e4)
    w = e2 * (u + v - q) / (2.0 * v)
    k = sqrt (u + v + w * w)
    d = k * sqrt (px * px + py * py) / (k + e2)
    h = ((k + e2 - 1.0) / k) * sqrt (d * d + pz * pz)

nvec :: Double -> Double -> Double -> Vector3d -> Vector3d
nvec d e2 k (Vector3d px py pz) = Vector3d nx ny nz
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx = s * a * px
    ny = s * a * py
    nz = s * pz
