{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- TODO: doc
--
module Data.Geo.Jord.Geodetics
    ( BearingDistance(..)
    , Geodetics(delta, destination)
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NedVector
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Positions
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Spherical
import Data.Geo.Jord.Transform

-- | Bearing and distance between 2 spherical positions.
newtype BearingDistance =
    BearingDistance (Angle, Length)
    deriving (Eq, Show)

instance Norm BearingDistance Length where
    norm (BearingDistance (_, l)) = l

class (Norm c Length) => Geodetics a b c where
    delta :: a -> a -> b -> c
    destination :: a -> c -> b -> a
    destination p0 d m
        | isZero (norm d :: Length) = p0
        | otherwise = _destination p0 d m
    -- private (not exported)
    _destination :: a -> c -> b -> a

-- | Ellipsoidal geodetics calculations on 'NVector's.
instance Geodetics NVector Ellipsoid NedVector where
     delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e

-- | Ellipsoidal geodetics calculations on 'LatLong's.
instance Geodetics LatLong Ellipsoid NedVector where
   delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e

-- | Ellipsoidal geodetics calculations on 'NVector's.
instance Geodetics NVectorPosition Ellipsoid NedVector where
     delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e

-- | Ellipsoidal geodetics calculations on 'NVector's.
instance Geodetics AngularPosition Ellipsoid NedVector where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e

-- | Ellipsoidal geodetics calculations on 'EcefPosition's.
instance Geodetics EcefPosition Ellipsoid NedVector where
     delta p1 p2 e = nedVectorMetres (nx r) (ny r) (nz r)
       where
         nv1 = NVector (toMetres (ex p1)) (toMetres (ey p1)) (toMetres (ez p1))
         nv2 = NVector (toMetres (ex p2)) (toMetres (ey p2)) (toMetres (ez p2))
         dpe = sub nv2 nv1
         n1 = fst (ecefToNVectorEllipsoidal p1 e)
         np = northPole
         d' = scale n1 (-1) -- down (pointing opposite to n-vector)
         e' = unit (cross np n1) -- east (pointing perpendicular to the plane)
         n' = cross e' d' -- north (by right hand rule)
         r = rotate dpe [n', e', d']

-- | Spherical geodetics calculations on 'NVector's.
instance Geodetics NVector Length BearingDistance where
    delta p1 p2 r = BearingDistance (initialBearing p1 p2, surfaceDistance p1 p2 r)
    _destination v (BearingDistance (b, d)) r =
        add (scale v (cos' ta)) (scale de (sin' ta))
      where
        ed = unit (cross northPole v) -- east direction vector at v
        nd = cross v ed -- north direction vector at v
        ta = central d r -- central angle
        de = add (scale nd (cos' b)) (scale ed (sin' b)) -- unit vector in the direction of the azimuth

-- | Spherical geodetics calculations on 'LatLong's.
instance Geodetics LatLong Length BearingDistance where
    delta p1 p2 r = delta (toNVector p1) (toNVector p2) r
    _destination p0 d r = fromNVector (_destination (toNVector p0) d r) 0.0

-- | Spherical geodetics calculations on 'NVectorPosition's.
instance Geodetics NVectorPosition Length BearingDistance where
    delta (NVectorPosition v1 _) (NVectorPosition v2 _) r = delta v1 v2 r
    _destination (NVectorPosition v h) d r = NVectorPosition (_destination v d r) h

-- | Spherical geodetics calculations on 'AngularPosition's.
instance Geodetics AngularPosition Length BearingDistance where
    delta p1 p2 r = delta (toNVector p1) (toNVector p2) r
    _destination (AngularPosition ll h) d r = fromNVector (_destination (toNVector ll) d r) h

-- | Spherical geodetics calculations on 'EcefPosition's.
instance Geodetics EcefPosition Length BearingDistance where
    delta p1 p2 r = delta v1 v2 r
      where
        v1 = fromEcef p1 r :: NVector
        v2 = fromEcef p2 r :: NVector
    _destination p0 d r = toEcef (_destination v d r) r
      where
        v = fromEcef p0 r :: NVector
