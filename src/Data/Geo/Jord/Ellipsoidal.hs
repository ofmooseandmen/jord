{-# LANGUAGE FlexibleInstances #-}

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
import Data.Geo.Jord.Earth
import Data.Geo.Jord.EcefPosition
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.NVector
import Data.Geo.Jord.NedVector
import Data.Geo.Jord.Quantity
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
        | norm d == zero = p0
        | otherwise = _target p0 d m
    -- private (not exported)
    _target :: a -> NedVector -> Ellipsoid -> a

-- | Ellipsoidal geodetics calculations on 'NVector's.
instance EGeodetics NVector where
    delta p1 p2 e = _delta p1 (toEcef p1 e) (toEcef p2 e)
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'LatLong's.
instance EGeodetics LatLong where
    delta p1 p2 e = _delta (toNVector p1) (toEcef p1 e) (toEcef p2 e)
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'NVector' 'AngularPosition's.
instance EGeodetics (AngularPosition NVector) where
    delta p1 p2 e = _delta (toNVector p1) (toEcef p1 e) (toEcef p2 e)
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'LatLong' 'AngularPosition's.
instance EGeodetics (AngularPosition LatLong) where
    delta p1 p2 e = _delta (toNVector p1) (toEcef p1 e) (toEcef p2 e)
    _target p0 d e = fromEcef (_target (toEcef p0 e) d e) e

-- | Ellipsoidal geodetics calculations on 'EcefPosition's.
instance EGeodetics EcefPosition where
    delta p1 p2 e = _delta (pos (ecefToNVectorEllipsoidal p1 e)) p1 p2
    _target p0@(EcefPosition p) (NedVector nv) e =
        ecefMetres (vx p + vx c) (vy p + vy c) (vz p + vz c)
      where
        n1 = vec (pos (ecefToNVectorEllipsoidal p0 e)) -- local (n-vector) coordinate frame
        a = vec northPole -- axis vector pointing to 90°
        d' = vscale n1 (-1) -- down (pointing opposite to n-vector)
        e' = vunit (vcross a n1) -- east (pointing perpendicular to the plane)
        n' = vcross e' d' -- north (by right hand rule)
        r =
            [ Vector3d (vx n') (vx e') (vx d')
            , Vector3d (vy n') (vy e') (vy d')
            , Vector3d (vz n') (vz e') (vz d')
            ] -- rotation matrix is built from n-vector coordinate frame axes (using column vectors)
        -- nv is NED delta to vector in coordinate frame of n-vector
        c = vrotate nv r -- apply rotation to nv to get delta in cartesian (ECEF) coordinate reference frame

_delta :: NVector -> EcefPosition -> EcefPosition -> NedVector
_delta (NVector nv1) (EcefPosition p1) (EcefPosition p2) = NedVector (vrotate dpe rm)
  where
    dpe = vsub p2 p1
      -- rotation matrix to go from Earth Frame to Normal Frame at p1
    np = vec northPole
    rd = vscale nv1 (-1) -- down (pointing opposite to n-vector)
    re = vunit (vcross np nv1) -- east (pointing perpendicular to the plane)
    rn = vcross re rd -- north (by right hand rule)
    rm = [rn, re, rd]
