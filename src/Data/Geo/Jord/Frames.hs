-- |
-- Module:      Data.Geo.Jord.Frames
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with reference frames
--
module Data.Geo.Jord.Frames
    ( FrameN(..)
    , FrameB(..)
    , FrameL(..)
    , Rotation(..)
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Vector3d

-- | North-East-Down (local level) frame.
--
--     * Position: The origin is directly beneath or above the vehicle (B), at Earth’s surface (surface
-- of ellipsoid model).
--
--     * Orientation: The x-axis points towards north, the y-axis points towards east (both are
-- horizontal), and the z-axis is pointing down.
--
--     * Comments: When moving relative to the Earth, the frame rotates about its z-axis to allow the
-- x-axis to always point towards north. When getting close to the poles this rotation rate
-- will increase, being infinite at the poles. The poles are thus singularities and the direction of
-- the x- and y-axes are not defined here. Hence, this coordinate frame is not suitable for
-- general calculations.
--
data FrameN =
    FrameN
    deriving (Eq, Show)

-- | Body frame (typically of a vehicle).
--
--     * Position: The origin is in the vehicle’s reference point.
--
--     * Orientation: The x-axis points forward, the y-axis to the right (starboard) and the z-axis
-- in the vehicle’s down direction.
--
--      * Comments: The frame is fixed to the vehicle.
--
data FrameB = FrameB
    { yaw :: Angle -- ^ body yaw angle (vertical axis)
    , pitch :: Angle -- ^ body pitch angle (transverse axis)
    , roll :: Angle -- ^ body roll angle (longitudinal axis)
    } deriving (Eq, Show)

-- | Local level, Wander azimuth frame.
--
--     * Position: The origin is directly beneath or above the vehicle (B), at Earth’s surface (surface
-- of ellipsoid model).
--
--     * Orientation: The z-axis is pointing down. Initially, the x-axis points towards north, and the
-- y-axis points towards east, but as the vehicle moves they are not rotating about the z-axis
-- (their angular velocity relative to the Earth has zero component along the z-axis).
-- (Note: Any initial horizontal direction of the x- and y-axes is valid for L, but if the
-- initial position is outside the poles, north and east are usually chosen for convenience.)
--
--     * Comments: The L-frame is equal to the N-frame except for the rotation about the z-axis,
-- which is always zero for this frame (relative to Earth). Hence, at a given time, the only
-- difference between the frames is an angle between the x-axis of L and the north direction;
-- this angle is called the wander azimuth angle. The L-frame is well suited for general
-- calculations, as it is non-singular.
--
newtype FrameL = FrameL
    { wanderAzimuth :: Angle
    } deriving (Eq, Show)

class Rotation a where
  earthToFrame :: Vector3d -> a -> [Vector3d]
  frameToEarth :: Vector3d -> a -> [Vector3d]
  frameToEarth v f = transpose (earthToFrame v f)

instance Rotation FrameN where
  earthToFrame v _ = rm
    where
      np = vec northPole
      rd = vscale v (-1) -- down (pointing opposite to n-vector)
      re = vunit (vcross np v) -- east (pointing perpendicular to the plane)
      rn = vcross re rd -- north (by right hand rule)
      rm = [rn, re, rd]

-- | transpose matrix made of 'Vector3d'.
transpose :: [Vector3d] -> [Vector3d]
transpose m = fmap l2v (transpose' xs)
   where
     xs = fmap v2l m

-- | transpose matrix.
transpose' :: [[Double]] -> [[Double]]
transpose' ([]:_) = []
transpose' x = map head x : transpose' (map tail x)

-- | 'Vector3d' to list of doubles.
v2l :: Vector3d -> [Double]
v2l (Vector3d x' y' z') = [x', y', z']

-- | list of doubles to 'Vector3d'.
l2v :: [Double] -> Vector3d
l2v [x', y', z'] = Vector3d x' y' z'
l2v xs = error ("Invalid list: " ++ show xs)
