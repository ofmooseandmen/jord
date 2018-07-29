{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:      Data.Geo.Jord.Ellipsoidal
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geodetic calculations assuming an __ellipsoidal__ earth model.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Ellipsoidal
    ( EGeodetics(delta, target)
    ) where

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

-- | Geodetic calculations assuming an ellipsoidal earth model.
class EGeodetics a where
    -- | @delta p1 p2 e@ computes the exact vector between the two positions @p1@ and @p2@, in north, east, and down.
    --
    -- Produced 'NedVector' is relative to @p1@: Due to the curvature of Earth and different directions to the North Pole,
    -- the north, east, and down directions will change (relative to Earth) for different places.
    --
    -- Position @p1@ must be outside the poles for the north and east directions to be defined.
    delta :: a -> a -> Ellipsoid -> NedVector
    -- | @target p0 d e@ computes the target position from position @p0@ and delta vector in north, east and down.
    --
    -- Position @p0@ must be outside the poles for the north and east directions to be defined.
    target :: a -> NedVector -> Ellipsoid -> a
    target p0 d m
        | vnorm d == 0 = p0
        | otherwise = _target p0 d m
    -- private (not exported)
    _target :: a -> NedVector -> Ellipsoid -> a

-- | Ellipsoidal geodetics calculations on 'NVector's.
instance EGeodetics NVector where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'LatLong's.
instance EGeodetics LatLong where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'NVector' 'AngularPosition's.
instance EGeodetics (AngularPosition NVector) where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'LatLong' 'AngularPosition's.
instance EGeodetics (AngularPosition LatLong) where
    delta p1 p2 e = delta (toEcef p1 e) (toEcef p2 e) e
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'EcefPosition's.
instance EGeodetics EcefPosition where
    delta p1 p2 e = nedMetres (nx r) (ny r) (nz r)
      where
        nv1 = NVector (toMetres (ex p1)) (toMetres (ey p1)) (toMetres (ez p1))
        nv2 = NVector (toMetres (ex p2)) (toMetres (ey p2)) (toMetres (ez p2))
        dpe = vsub nv2 nv1
        n1 = pos (ecefToNVectorEllipsoidal p1 e)
        np = northPole
        d' = vscale n1 (-1) -- down (pointing opposite to n-vector)
        e' = vunit (vcross np n1) -- east (pointing perpendicular to the plane)
        n' = vcross e' d' -- north (by right hand rule)
        r = vrotate dpe [n', e', d']
    _target p0@(EcefPosition x y z) d e =
        ecefMetres (toMetres x + nx c) (toMetres y + ny c) (toMetres z + nz c)
      where
        nv = NVector (toMetres (north d)) (toMetres (east d)) (toMetres (down d)) -- NED delta to vector in coordinate frame of n-vector
        n1 = pos (ecefToNVectorEllipsoidal p0 e) -- local (n-vector) coordinate frame
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
