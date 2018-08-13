-- |
-- Module:      Data.Geo.Jord.Rotation
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Rotation matrices from/to 3 angles about new axes
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Rotation
    ( r2xyz
    , r2zyx
    , xyz2r
    , zyx2r
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Vector3d

-- | Angles about new axes in the xyz-order from a rotation matrix.
--
-- The produced list contains 3 'Angle's of rotation about new axes.
--
-- The x, y, z angles are called Euler angles or Tait-Bryan angles and are
-- defined by the following procedure of successive rotations:
-- Given two arbitrary coordinate frames A and B. Consider a temporary frame
-- T that initially coincides with A. In order to make T align with B, we
-- first rotate T an angle x about its x-axis (common axis for both A and T).
-- Secondly, T is rotated an angle y about the NEW y-axis of T. Finally, T
-- is rotated an angle z about its NEWEST z-axis. The final orientation of
-- T now coincides with the orientation of B.
-- The signs of the angles are given by the directions of the axes and the
-- right hand rule.
r2xyz :: [Vector3d] -> [Angle]
r2xyz [v0, v1, v2] = [x, y, z]
  where
    v00 = vx v0
    v01 = vy v0
    v12 = vz v1
    v22 = vz v2
    z = atan2' (-v01) v00
    x = atan2' (-v12) v22
    sy = vz v0
    -- cos y is based on as many elements as possible, to average out
    -- numerical errors. It is selected as the positive square root since
    -- y: [-pi/2 pi/2]
    cy = sqrt ((v00 * v00 + v01 * v01 + v12 * v12 + v22 * v22) / 2.0)
    y = atan2' sy cy
r2xyz m = error ("Invalid rotation matrix " ++ show m)

-- | Angles about new axes in the xyz-order from a rotation matrix.
--
-- The produced list contains 3 'Angle's of rotation about new axes.
-- The z, x, y angles are called Euler angles or Tait-Bryan angles and are
-- defined by the following procedure of successive rotations:
-- Given two arbitrary coordinate frames A and B. Consider a temporary frame
-- T that initially coincides with A. In order to make T align with B, we
-- first rotate T an angle z about its z-axis (common axis for both A and T).
-- Secondly, T is rotated an angle y about the NEW y-axis of T. Finally, T
-- is rotated an angle x about its NEWEST x-axis. The final orientation of
-- T now coincides with the orientation of B.
-- The signs of the angles are given by the directions of the axes and the
-- right hand rule.
-- Note that if A is a north-east-down frame and B is a body frame, we
-- have that z=yaw, y=pitch and x=roll.
r2zyx :: [Vector3d] -> [Angle]
r2zyx m = [z, y, x]
  where
    [x, y, z] = fmap negate' (r2xyz (transpose m))

-- | Rotation matrix (direction cosine matrix) from 3 angles about new axes in the xyz-order.
--
-- The produced (no unit) rotation matrix is such
-- that the relation between a vector v decomposed in A and B is given by:
-- @v_A = mdot R_AB v_B@
--
-- The rotation matrix R_AB is created based on 3 'Angle's x,y,z about new axes
-- (intrinsic) in the order x-y-z. The angles are called Euler angles or
-- Tait-Bryan angles and are defined by the following procedure of successive
-- rotations:
-- Given two arbitrary coordinate frames A and B. Consider a temporary frame
-- T that initially coincides with A. In order to make T align with B, we
-- first rotate T an angle x about its x-axis (common axis for both A and T).
-- Secondly, T is rotated an angle y about the NEW y-axis of T. Finally, T
-- is rotated an angle z about its NEWEST z-axis. The final orientation of
-- T now coincides with the orientation of B.
-- The signs of the angles are given by the directions of the axes and the
-- right hand rule.
xyz2r :: Angle -> Angle -> Angle -> [Vector3d]
xyz2r x y z = [v1, v2, v3]
  where
    cx = cos' x
    sx = sin' x
    cy = cos' y
    sy = sin' y
    cz = cos' z
    sz = sin' z
    v1 = Vector3d (cy * cz) ((-cy) * sz) sy
    v2 = Vector3d (sy * sx * cz + cx * sz) ((-sy) * sx * sz + cx * cz) ((-cy) * sx)
    v3 = Vector3d ((-sy) * cx * cz + sx * sz) (sy * cx * sz + sx * cz) (cy * cx)

-- | rotation matrix (direction cosine matrix) from 3 angles about new axes in the zyx-order.
--
-- The produced (no unit) rotation matrix is such
-- that the relation between a vector v decomposed in A and B is given by:
-- @v_A = mdot R_AB v_B@
--
-- The rotation matrix R_AB is created based on 3 'Angle's
-- z,y,x about new axes (intrinsic) in the order z-y-x. The angles are called
-- Euler angles or Tait-Bryan angles and are defined by the following
-- procedure of successive rotations:
-- Given two arbitrary coordinate frames A and B. Consider a temporary frame
-- T that initially coincides with A. In order to make T align with B, we
-- first rotate T an angle z about its z-axis (common axis for both A and T).
-- Secondly, T is rotated an angle y about the NEW y-axis of T. Finally, T
-- is rotated an angle x about its NEWEST x-axis. The final orientation of
-- T now coincides with the orientation of B.
-- The signs of the angles are given by the directions of the axes and the
-- right hand rule.
--
-- Note that if A is a north-east-down frame and B is a body frame, we
-- have that z=yaw, y=pitch and x=roll.
zyx2r :: Angle -> Angle -> Angle -> [Vector3d]
zyx2r z y x = [v1, v2, v3]
  where
    cx = cos' x
    sx = sin' x
    cy = cos' y
    sy = sin' y
    cz = cos' z
    sz = sin' z
    v1 = Vector3d (cz * cy) ((-sz) * cx + cz * sy * sx) (sz * sx + cz * sy * cx)
    v2 = Vector3d (sz * cy) (cz * cx + sz * sy * sx) ((-cz) * sx + sz * sy * cx)
    v3 = Vector3d (-sy) (cy * sx) (cy * cx)
