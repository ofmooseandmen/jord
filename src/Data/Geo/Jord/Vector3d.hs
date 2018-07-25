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
    ) where

-- | operations on 3-element vectors.
class (Show a) => Vector3d a where
    -- | Adds 2 vectors.
    vadd :: a -> a -> a
    vadd v1 v2 = vector3d x y z
      where
        x = vecx v1 + vecx v2
        y = vecy v1 + vecy v2
        z = vecz v1 + vecz v2
    -- | Subtracts 2 vectors.
    vsub :: a -> a -> a
    vsub v1 v2 = vector3d x y z
      where
        x = vecx v1 - vecx v2
        y = vecy v1 - vecy v2
        z = vecz v1 - vecz v2
    -- | Computes the cross product of 2 vectors: the vector perpendicular to given vectors.
    vcross :: a -> a -> a
    vcross v1 v2 = vector3d x y z
      where
        x = vecy v1 * vecz v2 - vecz v1 * vecy v2
        y = vecz v1 * vecx v2 - vecx v1 * vecz v2
        z = vecx v1 * vecy v2 - vecy v1 * vecx v2
    -- | Computes the dot product of 2 vectors.
    vdot :: a -> a -> Double
    vdot v1 v2 = vecx v1 * vecx v2 + vecy v1 * vecy v2 + vecz v1 * vecz v2
    -- | Computes the norm of a vector.
    vnorm :: a -> Double
    vnorm v = sqrt (x * x + y * y + z * z)
      where
        x = vecx v
        y = vecy v
        z = vecz v
    -- | @vrotate v rm@ applies rotation matrix @rm@ to @v@.
    vrotate :: a -> [a] -> a
    vrotate v rm
        | length rm /= 3 = error ("Invalid rotation matrix" ++ show rm)
        | otherwise = vector3d x y z
      where
        [x, y, z] = map (vdot v) rm
    -- | @vscale v s@ multiplies each component of @v@ by @s@.
    vscale :: a -> Double -> a
    vscale v s = vector3d x y z
      where
        x = vecx v * s
        y = vecy v * s
        z = vecz v * s
    -- | Normalises a vector. The 'vnorm' of the produced vector is @1@.
    vunit :: a -> a
    vunit v
        | s == 1.0 = v
        | otherwise = vscale v s
      where
        s = 1.0 / vnorm v
    -- | first element of a vector.
    vecx :: a -> Double
    -- | second element of a vector.
    vecy :: a -> Double
    -- | third element of a vector.
    vecz :: a -> Double
    -- | vector from given elements.
    vector3d :: Double -> Double -> Double -> a
    -- | vector of vnorm 0.
    vzero :: a
    vzero = vector3d 0 0 0
