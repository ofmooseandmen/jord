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
-- TODO: doc
--
module Data.Geo.Jord.Transform
    ( VTransform(..)
    , ETransform(..)
    , HTransform(..)
    , nvectorToLatLong
    , latLongToNVector
    , nvectorToEcefEllipsoidal
    , ecefToNVectorEllipsoidal
    , nvectorToEcefSpherical
    , ecefToNVectorSpherical
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Positions
import Data.Geo.Jord.Quantity (norm)

-- | Transformation between positions and 'NVector'.
class VTransform a where
    toNVector :: GeoPos a b -> GeoPos NVector b -- ^ 'GeoPos' to 'NVector'.
    fromNVector :: Double -> GeoPos NVector b -> GeoPos a b -- ^ 'NVector' and height to 'GeoPos'.

-- | 'LatLong' <-> 'NVector'.
instance VTransform LatLong where
    toNVector (GeoPos ll m) = GeoPos (latLongToNVector ll) m
    fromNVector _ (GeoPos nv m) = GeoPos (nvectorToLatLong nv) m

-- | 'NVectorPosition' <-> 'NVector'.
instance VTransform NVectorPosition where
    toNVector (GeoPos (NVectorPosition nv _) m) = GeoPos nv m
    fromNVector h (GeoPos nv m) = GeoPos (NVectorPosition nv h) m

-- | 'AngularPosition' <-> 'NVector'.
instance VTransform AngularPosition where
    toNVector (GeoPos (AngularPosition ll _) m) = GeoPos (latLongToNVector ll) m
    fromNVector h (GeoPos nv m) = GeoPos (AngularPosition (nvectorToLatLong nv) h) m

-- | Transformation between 'EcefPosition' and angular or n-vector positions.
class ETransform a b where
    toEcef :: GeoPos a b -> GeoPos EcefPosition b
    fromEcef :: GeoPos EcefPosition b -> GeoPos a b

-- | Ellipsoidal transformation: 'NVector' <-> 'EcefPosition'.
instance ETransform NVector Ellipsoid where
    fromEcef (GeoPos p e) = GeoPos (fst (ecefToNVectorEllipsoidal p e)) e
    toEcef (GeoPos v e) = GeoPos (nvectorToEcefEllipsoidal v 0.0 e) e

-- | Spherical transformation: 'NVector' <-> 'EcefPosition'.
instance ETransform NVector Length where
    fromEcef (GeoPos p r) = GeoPos (fst (ecefToNVectorSpherical p r)) r
    toEcef (GeoPos v r) = GeoPos (nvectorToEcefSpherical v 0.0 r) r

-- | Ellipsoidal transformation: 'LatLong' <-> 'EcefPosition'.
instance ETransform LatLong Ellipsoid where
    fromEcef p = fromNVector 0.0 (fromEcef p :: (GeoPos NVector Ellipsoid))
    toEcef = toEcef . toNVector

-- | Spherical transformation: 'LatLong' <-> 'EcefPosition'.
instance ETransform LatLong Length where
    fromEcef p = fromNVector 0.0 (fromEcef p :: (GeoPos NVector Length))
    toEcef = toEcef . toNVector

-- | Ellipsoidal transformation: 'NVectorPosition' <-> 'EcefPosition'.
instance ETransform NVectorPosition Ellipsoid where
    fromEcef (GeoPos p e) = GeoPos (NVectorPosition nv h) e
      where
        (nv, h) = ecefToNVectorEllipsoidal p e
    toEcef (GeoPos (NVectorPosition v h) e) = GeoPos (nvectorToEcefEllipsoidal v h e) e

-- | Spherical transformation: 'NVectorPosition' <-> 'EcefPosition'.
instance ETransform NVectorPosition Length where
    fromEcef (GeoPos p r) = GeoPos (NVectorPosition nv h) r
      where
        (nv, h) = ecefToNVectorSpherical p r
    toEcef (GeoPos (NVectorPosition v h) r) = GeoPos (nvectorToEcefSpherical v h r) r

-- | Ellipsoidal transformation: 'AngularPosition' <-> 'EcefPosition'.
instance ETransform AngularPosition Ellipsoid where
    fromEcef (GeoPos p e) = GeoPos (AngularPosition (nvectorToLatLong nv) h) e
      where
        (nv, h) = ecefToNVectorEllipsoidal p e
    toEcef (GeoPos (AngularPosition ll h) e) =
        GeoPos (nvectorToEcefEllipsoidal (latLongToNVector ll) h e) e

-- | Spherical transformation: 'AngularPosition' <-> 'EcefPosition'.
instance ETransform AngularPosition Length where
    fromEcef (GeoPos p r) = GeoPos (AngularPosition (nvectorToLatLong nv) h) r
      where
        (nv, h) = ecefToNVectorSpherical p r
    toEcef (GeoPos (AngularPosition ll h) r) =
        GeoPos (nvectorToEcefSpherical (latLongToNVector ll) h r) r

-- | height of a geographic position.
class HTransform a b where
    height :: GeoPos a b -> Double

instance HTransform NVector b where
    height _ = 0.0

instance HTransform LatLong b where
    height _ = 0.0

instance HTransform NVectorPosition b where
    height (GeoPos (NVectorPosition _ h) _) = h

instance HTransform AngularPosition b where
    height (GeoPos (AngularPosition _ h) _) = h

instance HTransform EcefPosition Ellipsoid where
    height (GeoPos p e) = snd (ecefToNVectorEllipsoidal p e)

instance HTransform EcefPosition Length where
    height (GeoPos p r) = snd (ecefToNVectorSpherical p r)

nvectorToLatLong :: NVector -> LatLong
nvectorToLatLong v = latLong lat lon
  where
    lat = atan2' (nz v) (sqrt (nx v * nx v + ny v * ny v))
    lon = atan2' (ny v) (nx v)

latLongToNVector :: LatLong -> NVector
latLongToNVector g = NVector x' y' z'
  where
    lat = latitude g
    lon = longitude g
    cl = cos' lat
    x' = cl * cos' lon
    y' = cl * sin' lon
    z' = sin' lat

ecefToNVectorEllipsoidal :: EcefPosition -> Ellipsoid -> (NVector, Double)
ecefToNVectorEllipsoidal (EcefPosition x y z) e = (nvecEllipsoidal d e2 k px py pz, h)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = toMetres (equatorialRadius e)
    a2 = a * a
    px = toMetres x
    py = toMetres y
    pz = toMetres z
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
nvecEllipsoidal d e2 k px py pz = NVector nx' ny' nz'
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx' = s * a * px
    ny' = s * a * py
    nz' = s * pz

nvectorToEcefEllipsoidal :: NVector -> Double -> Ellipsoid -> EcefPosition
nvectorToEcefEllipsoidal v h e = EcefPosition ex' ey' ez'
  where
    nv = unit v
    a = toMetres (equatorialRadius e)
    b = toMetres (polarRadius e)
    nx' = nx nv
    ny' = ny nv
    nz' = nz nv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx' * nx' * m) + (ny' * ny' * m) + (nz' * nz'))
    ex' = metres (n * m * nx' + h * nx')
    ey' = metres (n * m * ny' + h * ny')
    ez' = metres (n * nz' + h * nz')

ecefToNVectorSpherical :: EcefPosition -> Length -> (NVector, Double)
ecefToNVectorSpherical p r = (NVector x y z, h)
  where
    n = toMetres (norm p)
    x = toMetres (ex p) / n
    y = toMetres (ey p) / n
    z = toMetres (ez p) / n
    h = n - toMetres r

nvectorToEcefSpherical :: NVector -> Double -> Length -> EcefPosition
nvectorToEcefSpherical v h r = EcefPosition x y z
  where
    nv = unit v
    n = h + toMetres r
    x = metres (n * nx nv)
    y = metres (n * ny nv)
    z = metres (n * nz nv)
