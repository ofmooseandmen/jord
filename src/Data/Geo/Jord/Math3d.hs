-- |
-- Module:      Data.Geo.Jord.Math3d
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- 3-element vector and associated math functions.
--
module Data.Geo.Jord.Math3d
    ( V3(..)
    , add
    , subtract
    , squaredDistance
    , dot
    , norm
    , cross
    , scale
    , unit
    , zero
    , transposeM
    , dotM
    , multM
    ) where

import Prelude hiding (subtract)

-- | 3-element vector.
data V3 =
    V3
        { vx :: Double
        , vy :: Double
        , vz :: Double
        }
    deriving (Eq, Show)

-- | Adds 2 vectors.
add :: V3 -> V3 -> V3
add v1 v2 = V3 x y z
  where
    x = vx v1 + vx v2
    y = vy v1 + vy v2
    z = vz v1 + vz v2

-- | Subtracts 2 vectors.
subtract :: V3 -> V3 -> V3
subtract v1 v2 = V3 x y z
  where
    x = vx v1 - vx v2
    y = vy v1 - vy v2
    z = vz v1 - vz v2

-- | Computes the cross product of 2 vectors: the vector perpendicular to given vectors.
cross :: V3 -> V3 -> V3
cross v1 v2 = V3 x y z
  where
    x = vy v1 * vz v2 - vz v1 * vy v2
    y = vz v1 * vx v2 - vx v1 * vz v2
    z = vx v1 * vy v2 - vy v1 * vx v2

-- | Computes the square of the straight line distance (or geometrical distance)
-- between 2 vectors.
squaredDistance :: V3 -> V3 -> Double
squaredDistance v1 v2 = dx * dx + dy * dy + dz * dz
  where
    dx = vx v1 - vx v2
    dy = vy v1 - vy v2
    dz = vz v1 - vz v2

-- | Computes the dot product of 2 vectors.
dot :: V3 -> V3 -> Double
dot v1 v2 = vx v1 * vx v2 + vy v1 * vy v2 + vz v1 * vz v2

-- | Computes the norm of a vector.
norm :: V3 -> Double
norm v = sqrt (x * x + y * y + z * z)
  where
    x = vx v
    y = vy v
    z = vz v

-- | Multiplies vector by __3x3__ matrix (rows).
multM :: V3 -> [V3] -> V3
multM v rm
    | length rm /= 3 = error ("Invalid matrix" ++ show rm)
    | otherwise = V3 x y z
  where
    [x, y, z] = map (dot v) rm

-- | @scale v s@ multiplies each component of @v@ by @s@.
scale :: V3 -> Double -> V3
scale v s = V3 x y z
  where
    x = vx v * s
    y = vy v * s
    z = vz v * s

-- | Normalises a vector. The 'norm' of the produced vector is @1@.
unit :: V3 -> V3
unit v
    | s == 1.0 = v
    | otherwise = scale v s
  where
    s = 1.0 / norm v

-- | vector of norm 0.
zero :: V3
zero = V3 0 0 0

-- | transpose __square (3x3)__ matrix of 'V3'.
transposeM :: [V3] -> [V3]
transposeM m = fmap ds2v (transpose' xs)
  where
    xs = fmap v2ds m

-- | transpose matrix.
transpose' :: [[Double]] -> [[Double]]
transpose' ([]:_) = []
transpose' x = map head x : transpose' (map tail x)

-- | multiplies 2 __3x3__ matrices.
dotM :: [V3] -> [V3] -> [V3]
dotM a b = fmap ds2v [[dot ar bc | bc <- transposeM b] | ar <- a]

-- | 'V3' to list of doubles.
v2ds :: V3 -> [Double]
v2ds (V3 x' y' z') = [x', y', z']

-- | list of doubles to 'V3'.
ds2v :: [Double] -> V3
ds2v [x', y', z'] = V3 x' y' z'
ds2v xs = error ("Invalid list: " ++ show xs)
