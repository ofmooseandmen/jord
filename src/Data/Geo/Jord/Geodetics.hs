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
-- Geodetic calculations for both __ellipsoidal__ and __spherical__ earth model.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Geodetics
    ( Geodetics(..)
    ) where

import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Earth
import Data.Geo.Jord.EcefPosition
import Data.Geo.Jord.Frames
import Data.Geo.Jord.Length
import Data.Geo.Jord.NedVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Transform
import Data.Geo.Jord.Vector3d

-- | Geodetics calculations.
class Geodetics a b where
    -- | @delta p1 p2 e@ computes the exact vector between the two positions @p1@ and @p2@, in north, east, and down.
    --
    -- Produced 'NedVector' is relative to @p1@: Due to the curvature of Earth and different directions to the North Pole,
    -- the north, east, and down directions will change (relative to Earth) for different places.
    --
    -- Position @p1@ must be outside the poles for the north and east directions to be defined.
    delta :: (ETransform a b) => a -> a -> b -> NedVector
    -- | @target p0 d e@ computes the target position from position @p0@ and delta vector in north, east and down.
    --
    -- Position @p0@ must be outside the poles for the north and east directions to be defined.
    target :: (ETransform a b) => a -> NedVector -> b -> a

instance Geodetics a Ellipsoid where
    delta p1 p2 e = delta' (envec p1 e) p1 p2 e
    target p0 d e = target' (envec p0 e) p0 d e

instance Geodetics a Length where
    delta p1 p2 e = delta' (snvec p1 e) p1 p2 e
    target p0 d e = target' (snvec p0 e) p0 d e

delta' :: (ETransform a b) => Vector3d -> a -> a -> b -> NedVector
delta' nv1 p1 p2 m = nedMetres (vx ned') (vy ned') (vz ned')
  where
    e1 = ecefvec p1 m
    e2 = ecefvec p2 m
    de = vsub e2 e1
    -- rotation matrix to go from Earth Frame to Normal Frame at p1
    rm = earthToFrame nv1 FrameN
    ned' = vrotate de rm

target' :: (ETransform a b) => Vector3d -> a -> NedVector -> b -> a
target' nv0 p0 d m
    | norm d == zero = p0
    | otherwise =  fromEcef (ecefMetres (vx e0 + vx c) (vy e0 + vy c) (vz e0 + vz c)) m
          where
            e0 = ecefvec p0 m
            rm = frameToEarth nv0 FrameN
            c = vrotate (vec d) rm -- apply rotation to nv to get delta in cartesian (ECEF) coordinate reference frame

ecefvec :: (ETransform a b) => a -> b -> Vector3d
ecefvec p m = vec (toEcef p m)

envec :: (ETransform a Ellipsoid) => a -> Ellipsoid -> Vector3d
envec p e = vec (pos (ecefToNVectorEllipsoidal (toEcef p e) e))

snvec :: (ETransform a Length) => a -> Length -> Vector3d
snvec p r = vec (pos (ecefToNVectorSpherical (toEcef p r) r))
