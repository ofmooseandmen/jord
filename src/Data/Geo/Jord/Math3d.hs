-- |
-- Module:      Data.Geo.Jord.Math3d
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- 3-element vector and associated math functions.
module Data.Geo.Jord.Math3d
    ( V3
    , v3x
    , v3y
    , v3z
    , vec3
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
        { v3x :: Double -- ^ x-coordinate
        , v3y :: Double -- ^ y-coordinate
        , v3z :: Double -- ^ z-coordinate
        }
    deriving (Eq, Show)

-- | Vector 3d from given coordinates.
-- 0.0 is added to each component to avoid @-0.0@.
vec3 :: Double -> Double -> Double -> V3
vec3 x y z = V3 (x + 0.0) (y + 0.0) (z + 0.0)

-- | Adds 2 vectors.
add :: V3 -> V3 -> V3
add (V3 x1 y1 z1) (V3 x2 y2 z2) = vec3 (x1 + x2) (y1 + y2) (z1 + z2)

-- | Subtracts 2 vectors.
subtract :: V3 -> V3 -> V3
subtract (V3 x1 y1 z1) (V3 x2 y2 z2) = vec3 (x1 - x2) (y1 - y2) (z1 - z2)

-- | Computes the cross product of 2 vectors: the vector perpendicular to given vectors.
cross :: V3 -> V3 -> V3
cross (V3 x1 y1 z1) (V3 x2 y2 z2) = vec3 x y z
  where
    x = y1 * z2 - z1 * y2
    y = z1 * x2 - x1 * z2
    z = x1 * y2 - y1 * x2

-- | Computes the square of the straight line distance (or geometrical distance)
-- between 2 vectors.
squaredDistance :: V3 -> V3 -> Double
squaredDistance (V3 x1 y1 z1) (V3 x2 y2 z2) = dx * dx + dy * dy + dz * dz
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

-- | Computes the dot product of 2 vectors.
dot :: V3 -> V3 -> Double
dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Computes the norm of a vector.
norm :: V3 -> Double
norm (V3 x y z) = sqrt (x * x + y * y + z * z)

-- | Multiplies vector by __3x3__ matrix (rows).
multM :: V3 -> [V3] -> V3
multM v rm
    | length rm /= 3 = error ("Invalid matrix" ++ show rm)
    | otherwise = vec3 x y z
  where
    [x, y, z] = map (dot v) rm

-- | @scale v s@ multiplies each component of @v@ by @s@.
scale :: V3 -> Double -> V3
scale (V3 x y z) s = vec3 (x * s) (y * s) (z * s)

-- | Normalises a vector. The 'norm' of the produced vector is @1@.
unit :: V3 -> V3
unit v
    | s == 1.0 = v
    | otherwise = scale v s
  where
    s = 1.0 / norm v

-- | vector of norm 0.
zero :: V3
zero = V3 0.0 0.0 0.0

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
v2ds (V3 x y z) = [x, y, z]

-- | list of doubles to 'V3'.
ds2v :: [Double] -> V3
ds2v [x, y, z] = vec3 x y z
ds2v xs = error ("Invalid list: " ++ show xs)
