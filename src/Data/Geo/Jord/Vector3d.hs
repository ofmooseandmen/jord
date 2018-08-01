-- |
-- Module:      Data.Geo.Jord.Vector3d
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- 3-element vectors.
--
module Data.Geo.Jord.Vector3d
    ( Vector3d(..)
    , IsVector3d(..)
    , vadd
    , vsub
    , vdot
    , vnorm
    , vcross
    , vrotate
    , vscale
    , vunit
    , vzero
    ) where

-- | 3-element vector.
data Vector3d = Vector3d
    { vx :: Double
    , vy :: Double
    , vz :: Double
    } deriving (Eq, Show)

-- | class for data types assimilated to 'Vector3d'.
class IsVector3d a where
    vec :: a -> Vector3d

-- | Adds 2 vectors.
vadd :: Vector3d -> Vector3d -> Vector3d
vadd v1 v2 = Vector3d x y z
  where
    x = vx v1 + vx v2
    y = vy v1 + vy v2
    z = vz v1 + vz v2

-- | Subtracts 2 vectors.
vsub :: Vector3d -> Vector3d -> Vector3d
vsub v1 v2 = Vector3d x y z
  where
    x = vx v1 - vx v2
    y = vy v1 - vy v2
    z = vz v1 - vz v2

-- | Computes the cross product of 2 vectors: the vector perpendicular to given vectors.
vcross :: Vector3d -> Vector3d -> Vector3d
vcross v1 v2 = Vector3d x y z
  where
    x = vy v1 * vz v2 - vz v1 * vy v2
    y = vz v1 * vx v2 - vx v1 * vz v2
    z = vx v1 * vy v2 - vy v1 * vx v2

-- | Computes the dot product of 2 vectors.
vdot :: Vector3d -> Vector3d -> Double
vdot v1 v2 = vx v1 * vx v2 + vy v1 * vy v2 + vz v1 * vz v2

-- | Computes the norm of a vector.
vnorm :: Vector3d -> Double
vnorm v = sqrt (x * x + y * y + z * z)
  where
    x = vx v
    y = vy v
    z = vz v

-- | @vrotate v rm@ applies rotation matrix @rm@ to @v@.
vrotate :: Vector3d -> [Vector3d] -> Vector3d
vrotate v rm
    | length rm /= 3 = error ("Invalid rotation matrix" ++ show rm)
    | otherwise = Vector3d x y z
  where
    [x, y, z] = map (vdot v) rm

-- | @vscale v s@ multiplies each component of @v@ by @s@.
vscale :: Vector3d -> Double -> Vector3d
vscale v s = Vector3d x y z
  where
    x = vx v * s
    y = vy v * s
    z = vz v * s

-- | Normalises a vector. The 'vnorm' of the produced vector is @1@.
vunit :: Vector3d -> Vector3d
vunit v
    | s == 1.0 = v
    | otherwise = vscale v s
  where
    s = 1.0 / vnorm v

-- | vector of vnorm 0.
vzero :: Vector3d
vzero = Vector3d 0 0 0
