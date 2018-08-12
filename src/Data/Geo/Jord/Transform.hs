{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Data.Geo.Jord.Transform
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Transformations between coordinates systems both in spherical and ellipsoidal form.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
module Data.Geo.Jord.Transform
    ( NTransform(..)
    , ETransform(..)
    , nvectorToLatLong
    , latLongToNVector
    , ecefToNVector
    , nvectorToEcef
    , geodeticHeight
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Earth
import Data.Geo.Jord.EcefPosition
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Vector3d

-- | Transformation between positions and 'AngularPosition' of 'NVector'.
class NTransform a where
    toNVector :: a -> AngularPosition NVector -- ^ position to 'AngularPosition' of 'NVector'.
    fromNVector :: AngularPosition NVector -> a -- ^ 'AngularPosition' of 'NVector' and height to position.

-- | 'NVector' <-> 'AngularPosition' of 'NVector'.
instance NTransform NVector where
    toNVector nv = AngularPosition nv zero
    fromNVector = pos

-- | 'LatLong' <-> 'AngularPosition' of 'NVector'.
instance NTransform LatLong where
    toNVector ll = AngularPosition (latLongToNVector ll) zero
    fromNVector = nvectorToLatLong . pos

-- | 'NTransform' identity.
instance NTransform (AngularPosition NVector) where
    toNVector = id
    fromNVector = id

-- | 'AngularPosition' of 'LatLong' <-> 'AngularPosition' of 'NVector'.
instance NTransform (AngularPosition LatLong) where
    toNVector (AngularPosition ll h) = AngularPosition (latLongToNVector ll) h
    fromNVector (AngularPosition nv h) = AngularPosition (nvectorToLatLong nv) h

-- | Transformation between 'EcefPosition' and angular or n-vector positions.
class ETransform a where
    toEcef :: a -> Earth -> EcefPosition -- ^ position and earth model to to 'EcefPosition'.
    fromEcef :: EcefPosition -> Earth -> a -- ^ 'EcefPosition' and earth model to position.

-- | 'NVector' <-> 'EcefPosition'.
instance ETransform NVector where
    fromEcef p e = pos (ecefToNVector p e)
    toEcef v = nvectorToEcef (nvectorHeight v zero)

-- | 'LatLong' <-> 'EcefPosition'.
instance ETransform LatLong where
    fromEcef p e = fromNVector (nvectorHeight (fromEcef p e :: NVector) zero)
    toEcef = toEcef . toNVector

-- | 'AngularPosition' of 'NVector' <-> 'EcefPosition'.
instance ETransform (AngularPosition NVector) where
    fromEcef = ecefToNVector
    toEcef = nvectorToEcef

-- | 'AngularPosition' of 'LatLong' <-> 'EcefPosition'.
instance ETransform (AngularPosition LatLong) where
    fromEcef p e = fromNVector (ecefToNVector p e)
    toEcef = nvectorToEcef . toNVector

-- | 'ETransform' identity.
instance ETransform EcefPosition where
    fromEcef p _ = p
    toEcef p _ = p

-- | @nvectorToLatLong v@ transforms 'NVector' @v@ to an equivalent 'LatLong'.
--
-- Same as 'toNVector'.
nvectorToLatLong :: NVector -> LatLong
nvectorToLatLong nv = latLong lat lon
  where
    v = vec nv
    lat = atan2' (vz v) (sqrt (vx v * vx v + vy v * vy v))
    lon = atan2' (vy v) (vx v)

-- | @latLongToNVector ll@ transforms 'LatLong' @ll@ to an equivalent 'NVector'.
--
-- See also 'fromNVector'.
latLongToNVector :: LatLong -> NVector
latLongToNVector ll = nvector x' y' z'
  where
    lat = latitude ll
    lon = longitude ll
    cl = cos' lat
    x' = cl * cos' lon
    y' = cl * sin' lon
    z' = sin' lat

-- | @ecefToNVector p e@ transforms 'EcefPosition' @p@ to an equivalent 'NVector' and geodetic height
-- using earth model @e@.
--
-- See also 'fromEcef'
ecefToNVector :: EcefPosition -> Earth -> AngularPosition NVector
-- Ellipsoidal
ecefToNVector ep e@(Ellipsoidal el) = nvectorHeight (nvecEllipsoidal d e2 k px py pz) (metres h)
  where
    ev = vec ep
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = toMetres (equatorialRadius el)
    a2 = a * a
    px = vx ev
    py = vy ev
    pz = vz ev
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
-- Spherical
ecefToNVector p (Spherical r) = nvectorHeight (nvector (vx nv) (vy nv) (vz nv)) h
  where
    ev = vec p
    nv = vunit ev
    h = sub (metres (vnorm ev)) r

nvecEllipsoidal :: Double -> Double -> Double -> Double -> Double -> Double -> NVector
nvecEllipsoidal d e2 k px py pz = nvector nx' ny' nz'
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx' = s * a * px
    ny' = s * a * py
    nz' = s * pz

-- | @nvectorToEcef (n, h) e@ transforms 'NVector' @n@ and geodetic height @h@
-- to an equivalent 'EcefPosition' using earth model @e@.
--
-- See also 'toEcef'
nvectorToEcef :: AngularPosition NVector -> Earth -> EcefPosition
-- Ellipsoidal
nvectorToEcef (AngularPosition nv h) e@(Ellipsoidal el) = ecef ex' ey' ez'
  where
    v = vec nv
    uv = vunit v
    a = toMetres (equatorialRadius el)
    b = toMetres (polarRadius e)
    nx' = vx uv
    ny' = vy uv
    nz' = vz uv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx' * nx' * m) + (ny' * ny' * m) + (nz' * nz'))
    h' = toMetres h
    ex' = metres (n * m * nx' + h' * nx')
    ey' = metres (n * m * ny' + h' * ny')
    ez' = metres (n * nz' + h' * nz')
-- Spherical
nvectorToEcef (AngularPosition nv h) (Spherical r) = ecefMetres (vx ev) (vy ev) (vz ev)
  where
    unv = vunit . vec $ nv
    n = add h r
    ev = vscale unv (toMetres n)

-- | @geodeticHeight p e@ computes the geodetic height of 'EcefPosition' @p@ using earth model @e@.
--
-- The geodetic height (or ellipsoidal height) is __not__ the mean sea level (MSL) height.
geodeticHeight :: EcefPosition -> Earth -> Length
geodeticHeight p e = height (ecefToNVector p e)
