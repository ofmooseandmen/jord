{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
    , ecefToNVectorEllipsoidal
    , ecefToNVectorSpherical
    , nvectorToEcefEllipsoidal
    , nvectorToEcefSpherical
    , geodeticHeight
    , sphericalHeight
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

-- | Transformation between positions and 'NVector'.
class NTransform a where
    toNVector :: a -> NVector -- ^ position to 'NVector'.
    fromNVector :: NVector -> Length -> a -- ^ 'NVector' and height to position.

-- | 'LatLong' <-> 'NVector'.
instance NTransform LatLong where
    toNVector = latLongToNVector
    fromNVector nv _ = nvectorToLatLong nv

-- | 'AngularPosition' of 'NVector' <-> 'NVector'.
instance NTransform (AngularPosition NVector) where
    toNVector = pos
    fromNVector = AngularPosition

-- | 'AngularPosition' of 'LatLong' <-> 'NVector'.
instance NTransform (AngularPosition LatLong) where
    toNVector = latLongToNVector . pos
    fromNVector nv = AngularPosition (nvectorToLatLong nv)

-- | Transformation between 'EcefPosition' and angular or n-vector positions.
class ETransform a b where
    toEcef :: a -> b -> EcefPosition -- ^ position and earth model to to 'EcefPosition'.
    fromEcef :: EcefPosition -> b -> a -- ^ 'EcefPosition' and earth model to position.

-- | Ellipsoidal transformation: 'NVector' <-> 'EcefPosition'.
instance ETransform NVector Ellipsoid where
    fromEcef p e = pos (ecefToNVectorEllipsoidal p e)
    toEcef v = nvectorToEcefEllipsoidal (nvectorHeight v zero)

-- | Spherical transformation: 'NVector' <-> 'EcefPosition'.
instance ETransform NVector Length where
    fromEcef p r = pos (ecefToNVectorSpherical p r)
    toEcef v = nvectorToEcefSpherical (nvectorHeight v zero)

-- | Ellipsoidal transformation: 'LatLong' <-> 'EcefPosition'.
instance ETransform LatLong Ellipsoid where
    fromEcef p e = fromNVector (fromEcef p e :: NVector) zero
    toEcef = toEcef . toNVector

-- | Spherical transformation: 'LatLong' <-> 'EcefPosition'.
instance ETransform LatLong Length where
    fromEcef p r = fromNVector (fromEcef p r :: NVector) zero
    toEcef = toEcef . toNVector

-- | Ellipsoidal transformation: 'AngularPosition' of 'NVector' <-> 'EcefPosition'.
instance ETransform (AngularPosition NVector) Ellipsoid where
    fromEcef = ecefToNVectorEllipsoidal
    toEcef = nvectorToEcefEllipsoidal

-- | Spherical transformation: 'AngularPosition' of 'NVector' <-> 'EcefPosition'.
instance ETransform (AngularPosition NVector) Length where
    fromEcef = ecefToNVectorSpherical
    toEcef = nvectorToEcefSpherical

-- | Ellipsoidal transformation: 'AngularPosition' of 'LatLong' <-> 'EcefPosition'.
instance ETransform (AngularPosition LatLong) Ellipsoid where
    fromEcef p e = fromNVector (pos nvh) (height nvh)
      where
        nvh = ecefToNVectorEllipsoidal p e
    toEcef (AngularPosition ll h) = nvectorToEcefEllipsoidal (nvectorHeight (toNVector ll) h)

-- | Spherical transformation: 'AngularPosition' of 'LatLong' <-> 'EcefPosition'.
instance ETransform (AngularPosition LatLong) Length where
    fromEcef p r = fromNVector (pos nvh) (height nvh)
      where
        nvh = ecefToNVectorSpherical p r
    toEcef (AngularPosition ll h) = nvectorToEcefSpherical (nvectorHeight (toNVector ll) h)

-- | @nvectorToLatLong v@ transforms 'NVector' @v@ to an equivalent 'LatLong'.
--
-- Same as 'toNVector'.
nvectorToLatLong :: NVector -> LatLong
nvectorToLatLong (NVector v) = latLong lat lon
  where
    lat = atan2' (vz v) (sqrt (vx v * vx v + vy v * vy v))
    lon = atan2' (vy v) (vx v)

-- | @latLongToNVector ll@ transforms 'LatLong' @ll@ to an equivalent 'NVector'.
--
-- See also 'fromNVector'.
latLongToNVector :: LatLong -> NVector
latLongToNVector ll = NVector (Vector3d x' y' z')
  where
    lat = latitude ll
    lon = longitude ll
    cl = cos' lat
    x' = cl * cos' lon
    y' = cl * sin' lon
    z' = sin' lat

-- | @ecefToNVectorEllipsoidal p e@ transforms 'EcefPosition' @p@ to an equivalent 'NVector' and geodetic height
-- using ellipsoid @e@.
--
-- See also 'fromEcef'
ecefToNVectorEllipsoidal :: EcefPosition -> Ellipsoid -> AngularPosition NVector
ecefToNVectorEllipsoidal (EcefPosition ev) e =
    nvectorHeight (nvecEllipsoidal d e2 k px py pz) (metres h)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = toMetres (equatorialRadius e)
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

nvecEllipsoidal :: Double -> Double -> Double -> Double -> Double -> Double -> NVector
nvecEllipsoidal d e2 k px py pz = NVector (Vector3d nx' ny' nz')
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx' = s * a * px
    ny' = s * a * py
    nz' = s * pz

-- | @nvectorToEcefEllipsoidal (n, h) e@ transforms 'NVector' @n@ and geodetic height @h@
-- to an equivalent 'EcefPosition' using ellipsoid @e@.
--
-- See also 'toEcef'
nvectorToEcefEllipsoidal :: AngularPosition NVector -> Ellipsoid -> EcefPosition
nvectorToEcefEllipsoidal (AngularPosition (NVector nv) h) e = ecef ex' ey' ez'
  where
    unv = vunit nv
    a = toMetres (equatorialRadius e)
    b = toMetres (polarRadius e)
    nx' = vx unv
    ny' = vy unv
    nz' = vz unv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx' * nx' * m) + (ny' * ny' * m) + (nz' * nz'))
    h' = toMetres h
    ex' = metres (n * m * nx' + h' * nx')
    ey' = metres (n * m * ny' + h' * ny')
    ez' = metres (n * nz' + h' * nz')

-- | @ecefToNVectorSpherical p r@ transforms 'EcefPosition' @p@ to an equivalent 'NVector' and height
-- using mean earth radius @r@.
--
-- See also 'fromEcef'
ecefToNVectorSpherical :: EcefPosition -> Length -> AngularPosition NVector
ecefToNVectorSpherical (EcefPosition ev) r = nvectorHeight (NVector nv) h
  where
    nv = vunit ev
    h = sub (metres (vnorm ev)) r

-- | @nvectorToEcefSpherical (n, h) r@ transforms 'NVector' @n@ and height @h@
-- to an equivalent 'EcefPosition' using mean earth radius @r@.
--
-- See also 'toEcef'
nvectorToEcefSpherical :: AngularPosition NVector -> Length -> EcefPosition
nvectorToEcefSpherical (AngularPosition (NVector nv) h) r = EcefPosition ev
  where
    unv = vunit nv
    n = add h r
    ev = vscale unv (toMetres n)

-- | @geodeticHeight p e@ computes the geodetic height of 'EcefPosition' @p@ using ellipsoid @e@.
--
-- The geodetic height (or ellipsoidal height) is __not__ the mean sea level (MSL) height.
geodeticHeight :: EcefPosition -> Ellipsoid -> Length
geodeticHeight p e = height (ecefToNVectorEllipsoidal p e)

-- | @sphericalHeight p r@ computes the height of 'EcefPosition' @p@ using mean earth radius @r@.
sphericalHeight :: EcefPosition -> Length -> Length
sphericalHeight p r = height (ecefToNVectorSpherical p r)
