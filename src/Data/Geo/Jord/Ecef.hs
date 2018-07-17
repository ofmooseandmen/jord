-- |
-- Module:      Data.Geo.Jord.Ecef
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working earth-centered, earth-fixed (ECEF) positions.
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
geodeticToEcef (p, h) e = rotate (Vector3d ex ey ez) [Vector3d 0 0 (-1), Vector3d 0 1 0, Vector3d 1 0 0]
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
ecefToGeodetic p'@(Vector3d px py pz) e = (fromNVector (nvec d e2 k p'), height d e2 k px)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e' ** 4
    a = toMetres (equatorialRadius e)
    q = ((1 - e2) / (a * a)) * (px * px)
    p = (py * px / pz * pz) / (a * a)
    r = (p + q - e4) / 6.0
    s = (e4 * p * q) / (4 * r ** 3)
    t = (1.0 + s + sqrt (s * (2.0 + s))) ** (1 / 3)
    u = r * (1.0 + t + 1.0 / t)
    v = sqrt (u * u + q * e4)
    w = e2 * (u + v - q) / (2.0 * v)
    k = sqrt (u + v + w * w)
    d = k * sqrt (py * py + pz * pz) / (k + e2)

    -- var a = datum.ellipsoid.a;
    -- var f = datum.ellipsoid.f;
    --
    -- var x = this.x;
    -- var y = this.y;
    -- var z = this.z;
    --
    -- var e2 = 2*f - f*f; // eÂ² = 1st eccentricity squared â‰¡ (aÂ²-bÂ²)/aÂ²
    -- var e4 = e2*e2;     // eâ´
    --
    -- var p = (x*x + y*y) / (a*a);
    -- var q = z*z * (1-e2) / (a*a);
    -- var r = (p + q - e4) / 6;
    -- var s = (e4*p*q) / (4*r*r*r);
    -- var t = Math.cbrt(1 + s + Math.sqrt(2*s+s*s));
    -- var u = r * (1 + t + 1/t);
    -- var v = Math.sqrt(u*u + e4*q);
    -- var w = e2 * (u + v - q) / (2*v);
    -- var k = Math.sqrt(u + v + w*w) - w;
    -- var d = k * Math.sqrt(x*x + y*y) / (k + e2);
    --
    -- var tmp = 1 / Math.sqrt(d*d + z*z);
    -- var xÊ¹ = tmp * k/(k+e2) * x;
    -- var yÊ¹ = tmp * k/(k+e2) * y;
    -- var zÊ¹ = tmp * z;
    -- var h = (k + e2 - 1)/k * Math.sqrt(d*d + z*z);
    --
    -- var n = new NvectorEllipsoidal(xÊ¹, yÊ¹, zÊ¹, h, datum);


nvec :: Double -> Double -> Double -> Vector3d -> Vector3d
nvec d e2 k (Vector3d px py pz) = scale (Vector3d px (a * py) (a * pz)) s
  where
    s = 1.0 / sqrt (d * d + px * px)
    a = k / (k + e2)

height :: Double -> Double -> Double -> Double -> Double
height d e2 k px = ((k + e2 - 1.0) / k) * sqrt (d * d + px * px)
