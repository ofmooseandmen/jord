{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:      Data.Geo.Jord.Geodetics
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Functions to solve the <https://en.wikipedia.org/wiki/Geodesy#Geodetic_problems Geodetic problems>
-- for both ellipsoidal and spherical earth models.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Geodetics
    ( BearingDistance(..)
    , Geodetics(delta, destination)
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.EcefPosition
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.NedVector
import Data.Geo.Jord.Spherical
import Data.Geo.Jord.Transform
import Data.Geo.Jord.Vector3d

-- | Bearing and distance between 2 spherical positions.
newtype BearingDistance =
    BearingDistance (Angle, Length)
    deriving (Eq, Show)

class IsZero a where
    isZero :: a -> Bool

instance IsZero BearingDistance where
    isZero (BearingDistance (_, l)) = toMetres l == 0.0

instance IsZero NedVector where
    isZero v = vnorm v == 0.0

-- | Class 'Geodetics' defines two functions each solving a geodetic problem.
class (IsZero c) => Geodetics a b c where
    -- | @delta p1 p2 m@ computes the delta between @p1@ and @p2@ using the earth model @m@.
    --
    -- This is know as the second (inverse or reverse) geodetic problem.
    delta :: a -> a -> b -> c
    -- | @destination p0 d m@ computes the destination point from position @p0@ and delta @d@
    -- using the earth model @m@.
    --
    -- This is know as the first (direct or forward) geodetic problem.
    destination :: a -> c -> b -> a
    destination p0 d m
        | isZero d = p0
        | otherwise = _destination p0 d m
    -- private (not exported)
    _destination :: a -> c -> b -> a

-- | Ellipsoidal geodetics calculations on 'NVector's.
--
-- deltas are expressed as 'NedVector's relative to the first position.
instance Geodetics NVector Ellipsoid NedVector where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _destination p0 d e = fromEcef (_destination (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'LatLong's.
--
-- deltas are expressed as 'NedVector's relative to the first position.
instance Geodetics LatLong Ellipsoid NedVector where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _destination p0 d e = fromEcef (_destination (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'NVector' 'AngularPosition's.
--
-- deltas are expressed as 'NedVector's relative to the first position.
instance Geodetics (AngularPosition NVector) Ellipsoid NedVector where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _destination p0 d e = fromEcef (_destination (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'LatLong' 'AngularPosition's.
--
-- deltas are expressed as 'NedVector's relative to the first position.
instance Geodetics (AngularPosition LatLong) Ellipsoid NedVector where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _destination p0 d e = fromEcef (_destination (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'EcefPosition's.
--
-- deltas are expressed as 'NedVector's relative to the first position.
instance Geodetics EcefPosition Ellipsoid NedVector where
    delta p1 p2 e = nedVectorMetres (nx r) (ny r) (nz r)
      where
        nv1 = NVector (toMetres (ex p1)) (toMetres (ey p1)) (toMetres (ez p1))
        nv2 = NVector (toMetres (ex p2)) (toMetres (ey p2)) (toMetres (ez p2))
        dpe = vsub nv2 nv1
        n1 = fst (ecefToNVectorEllipsoidal p1 e)
        np = northPole
        d' = vscale n1 (-1) -- down (pointing opposite to n-vector)
        e' = vunit (vcross np n1) -- east (pointing perpendicular to the plane)
        n' = vcross e' d' -- north (by right hand rule)
        r = vrotate dpe [n', e', d']
    _destination p0@(EcefPosition x y z) d e =
        ecefPosMetres (toMetres x + nx c) (toMetres y + ny c) (toMetres z + nz c)
      where
        nv = NVector (toMetres (north d)) (toMetres (east d)) (toMetres (down d)) -- NED delta to vector in coordinate frame of n-vector
        n1 = fst (ecefToNVectorEllipsoidal p0 e) -- local (n-vector) coordinate frame
        a = northPole -- axis vector pointing to 90°
        d' = vscale n1 (-1) -- down (pointing opposite to n-vector)
        e' = vunit (vcross a n1) -- east (pointing perpendicular to the plane)
        n' = vcross e' d' -- north (by right hand rule)
        r =
            [ NVector (nx n') (nx e') (nx d')
            , NVector (ny n') (ny e') (ny d')
            , NVector (nz n') (nz e') (nz d')
            ] -- rotation matrix is built from n-vector coordinate frame axes (using column vectors)
        c = vrotate nv r -- apply rotation to nv to get delta in cartesian (ECEF) coordinate reference frame

-- | Spherical geodetics calculations on 'NVector's.
--
-- deltas are expressed as 'BearingDistance's.
instance Geodetics NVector Length BearingDistance where
    delta p1 p2 r = BearingDistance (initialBearing p1 p2, surfaceDistance p1 p2 r)
    _destination v (BearingDistance (b, d)) r = vadd (vscale v (cos' ta)) (vscale de (sin' ta))
      where
        ed = vunit (vcross northPole v) -- east direction vector at v
        nd = vcross v ed -- north direction vector at v
        ta = central d r -- central angle
        de = vadd (vscale nd (cos' b)) (vscale ed (sin' b)) -- vunit vector in the direction of the azimuth

-- | Spherical geodetics calculations on 'LatLong's.
--
-- deltas are expressed as 'BearingDistance's.
instance Geodetics LatLong Length BearingDistance where
    delta p1 p2 = delta (toNVector p1) (toNVector p2)
    _destination p0 d r = fromNVector (_destination (toNVector p0) d r) 0.0

-- | Spherical geodetics calculations on 'NVector' 'AngularPosition's.
--
-- deltas are expressed as 'BearingDistance's.
instance Geodetics (AngularPosition NVector) Length BearingDistance where
    delta (AngularPosition v1 _) (AngularPosition v2 _) = delta v1 v2
    _destination (AngularPosition v h) d r = AngularPosition (_destination v d r) h

-- | Spherical geodetics calculations on 'LatLong' 'AngularPosition's.
--
-- deltas are expressed as 'BearingDistance's.
instance Geodetics (AngularPosition LatLong) Length BearingDistance where
    delta p1 p2 = delta (toNVector p1) (toNVector p2)
    _destination (AngularPosition ll h) d r = fromNVector (_destination (toNVector ll) d r) h

-- | Spherical geodetics calculations on 'EcefPosition's.
--
-- deltas are expressed as 'BearingDistance's.
instance Geodetics EcefPosition Length BearingDistance where
    delta p1 p2 r = delta v1 v2 r
      where
        v1 = fromEcef p1 r :: NVector
        v2 = fromEcef p2 r :: NVector
    _destination p0 d r = toEcef (_destination v d r) r
      where
        v = fromEcef p0 r :: NVector
