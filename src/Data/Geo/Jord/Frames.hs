{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Data.Geo.Jord.Frames
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Type and functions for working with delta vectors in difference reference frames.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Frames
    (
    -- * Reference Frames
      Frame(..)
    , FrameB
    , FrameL
    , FrameN
    , frameN
    -- * Deltas
    , Delta
    , delta
    , deltaMetres
    -- * Delta in the north, east, down frame.
    , Ned
    , ned
    , nedMetres
    , north
    , east
    , down
    , bearing
    , elevation
    , norm
    -- * Calculations
    , deltaBetween
    , target
    , targetN
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Earth
import Data.Geo.Jord.EcefPosition
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Transform
import Data.Geo.Jord.Vector3d

-- | class for reference frames.
class Frame a where
    earthToFrame :: a -> [Vector3d] -- ^ rotation matrix to go from Earth-Fixed frame to frame @a@.
    frameToEarth :: a -> [Vector3d] -- ^ rotation matrix to go from frame @a@ to Earth-Fixed frame.
    frameToEarth = transpose . earthToFrame

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
newtype FrameN =
    FrameN Vector3d
    deriving (Eq, Show)

instance Frame FrameN where
    earthToFrame (FrameN o) = rm
      where
        np = vec northPole
        rd = vscale o (-1) -- down (pointing opposite to n-vector)
        re = vunit (vcross np o) -- east (pointing perpendicular to the plane)
        rn = vcross re rd -- north (by right hand rule)
        rm = [rn, re, rd]

frameN :: (ETransform a) => a -> Earth -> FrameN
frameN p e = FrameN (nvec p e)

-- | delta between position in one of the reference frames.
newtype Delta =
    Delta Vector3d
    deriving (Eq, Show)

-- | 'Delta' from given x, y and z length.
delta :: Length -> Length -> Length -> Delta
delta x y z = Delta (Vector3d (toMetres x) (toMetres y) (toMetres z))

-- | 'Delta' from given x, y and z length in _metres_.
deltaMetres :: Double -> Double -> Double -> Delta
deltaMetres x y z = delta (metres x) (metres y) (metres z)

-- | North, east and down delta (thus in frame 'FrameN').
newtype Ned =
    Ned Vector3d
    deriving (Eq, Show)

-- | 'Ned' from given north, east and down.
ned :: Length -> Length -> Length -> Ned
ned n e d = Ned (Vector3d (toMetres n) (toMetres e) (toMetres d))

-- | 'Ned' from given north, east and down in _metres_.
nedMetres :: Double -> Double -> Double -> Ned
nedMetres n e d = ned (metres n) (metres e) (metres d)

-- | North component of given 'Ned'.
north :: Ned -> Length
north (Ned v) = metres (vx v)

-- | East component of given 'Ned'.
east :: Ned -> Length
east (Ned v) = metres (vy v)

-- | Down component of given 'Ned'.
down :: Ned -> Length
down (Ned v) = metres (vz v)

-- | @bearing v@ computes the bearing of the NED vector @v@ from north.
bearing :: Ned -> Angle
bearing v =
    let a = atan2' (toMetres (east v)) (toMetres (north v))
     in normalise a (decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED vector @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: Ned -> Angle
elevation (Ned v) = negate' (asin' (vz v / vnorm v))

-- | @norm v@ computes the norm of the NED vector @v@.
norm :: Ned -> Length
norm (Ned v) = metres (vnorm v)

-- | @delta p1 p2 m@ computes the exact vector between the two positions @p1@ and @p2@, in north, east, and down.
--
-- Produced 'Ned' delta is relative to @p1@: Due to the curvature of Earth and different directions to the North Pole,
-- the north, east, and down directions will change (relative to Earth) for different places.
--
-- Position @p1@ must be outside the poles for the north and east directions to be defined.
deltaBetween :: (ETransform a) => a -> a -> Earth -> Ned
deltaBetween p1 p2 e = nedMetres (vx d) (vy d) (vz d)
  where
    e1 = ecefvec p1 e
    e2 = ecefvec p2 e
    de = vsub e2 e1
    -- rotation matrix to go from Earth Frame to Normal Frame at p1
    f = frameN p1 e
    rm = earthToFrame f
    d = vrotate de rm

-- | @target p0 f d m@ computes the target position from position @p0@ and delta @d@ using in frame @f@
-- and using earth model @e@.
target :: (ETransform a, Frame c) => a -> (a -> Earth -> c) -> Delta -> Earth -> a
target p0 f (Delta d) e = fromEcef (ecefMetres (vx e0 + vx c) (vy e0 + vy c) (vz e0 + vz c)) e
  where
    e0 = ecefvec p0 e
    rm = frameToEarth (f p0 e)
    c = vrotate d rm

-- | @targetN p0 d m@ computes the target position from position @p0@ and north, east, down @d@ using earth model @e@.
targetN :: (ETransform a) => a -> Ned -> Earth -> a
targetN p0 (Ned d) = target p0 frameN (Delta d)

-- | ECEF position (as a 'Vector3d') from given position.
ecefvec :: (ETransform a) => a -> Earth -> Vector3d
ecefvec p m = vec (toEcef p m)

-- | NVector (as a 'Vector3d') from given positon.
nvec :: (ETransform a) => a -> Earth -> Vector3d
nvec p e = vec (pos (ecefToNVector (toEcef p e) e))

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
