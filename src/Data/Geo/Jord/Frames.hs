-- |
-- Module:      Data.Geo.Jord.Frames
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Type and functions for working with delta vectors in different reference frames.
--
-- Note: though the API accept spherical models, doing so defeats the purpose of this module
-- which is to find exact solutions. Prefer using ellipsoidal models.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
module Data.Geo.Jord.Frames
    (
    -- * Reference Frames
      Frame(..)
    -- * Body frame
    , FrameB
    , yaw
    , pitch
    , roll
    , bOrigin
    , frameB
    -- * Local frame
    , FrameL
    , wanderAzimuth
    , lOrigin
    , frameL
    -- * North-East-Down frame
    , FrameN
    , nOrigin
    , frameN
    -- * Deltas
    , Delta
    , delta
    , deltaMetres
    , dx
    , dy
    , dz
    -- * Delta in the north, east, down frame
    , Ned
    , ned
    , nedMetres
    , north
    , east
    , down
    , bearing
    , elevation
    , slantRange
    -- * Calculations
    , deltaBetween
    , nedBetween
    , target
    , targetN
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Bodies
import Data.Geo.Jord.Internal
import Data.Geo.Jord.Length
import Data.Geo.Jord.Position
import Data.Geo.Jord.Rotation
import Data.Geo.Jord.Vector3d

-- | class for reference frames.
--
-- Supported frames:
--
--     * 'FrameB': 'rEF' returns R_EB
--
--     * 'FrameL': 'rEF' returns R_EL
--
--     * 'FrameN': 'rEF' returns R_EN
--
class Frame a where
    rEF :: a -> [Vector3d] -- ^ rotation matrix to transform vectors decomposed in frame @a@ to vectors decomposed Earth-Fixed frame.

-- | Body frame (typically of a vehicle).
--
--     * Position: The origin is in the vehicle’s reference point.
--
--     * Orientation: The x-axis points forward, the y-axis to the right (starboard) and the z-axis
-- in the vehicle’s down direction.
--
--      * Comments: The frame is fixed to the vehicle.
--
data FrameB a =
    FrameB
        { yaw :: Angle -- ^ body yaw angle (vertical axis).
        , pitch :: Angle -- ^ body pitch angle (transverse axis).
        , roll :: Angle -- ^ body roll angle (longitudinal axis).
        , bOrigin :: Position a -- ^ frame origin.
        } deriving (Eq, Show)

-- | 'FrameB' from given yaw, pitch, roll, position (origin).
frameB :: (Model a) => Angle -> Angle -> Angle -> Position a -> FrameB a
frameB = FrameB

-- | R_EB: frame B to Earth
instance Frame (FrameB a) where
    rEF (FrameB y p r o) = rm
      where
        rNB = zyx2r y p r
        n = FrameN o
        rEN = rEF n
        rm = mdot rEN rNB -- closest frames cancel: N

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
data FrameL a =
    FrameL
        { wanderAzimuth :: Angle -- ^ wander azimuth: angle between x-axis of the frame L and the north direction.
        , lOrigin :: Position a -- ^ frame origin.
        } deriving (Eq, Show)

-- | R_EL: frame L to Earth
instance Frame (FrameL m) where
    rEF (FrameL w o) = rm
      where
        lat = latitude o
        lon = longitude o
        r = xyz2r lon (negate' lat) w
        rEe' = [Vector3d 0 0 (-1), Vector3d 0 1 0, Vector3d 1 0 0]
        rm = mdot rEe' r

-- | 'FrameL' from given wander azimuth, position (origin).
frameL :: (Model a) => Angle -> Position a -> FrameL a
frameL = FrameL

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
newtype FrameN a =
    FrameN
        { nOrigin :: Position a -- ^ frame origin.
        } deriving (Eq, Show)

-- | R_EN: frame N to Earth
instance Frame (FrameN a) where
    rEF (FrameN o) = transpose rm
      where
        vo = nvec o
        np = nvNorthPole
        rd = vscale vo (-1) -- down (pointing opposite to n-vector)
        re = vunit (vcross np vo) -- east (pointing perpendicular to the plane)
        rn = vcross re rd -- north (by right hand rule)
        rm = [rn, re, rd]

-- | 'FrameN' from given position (origin).
frameN :: (Model a) => Position a -> FrameN a
frameN = FrameN

-- | delta between position in one of the reference frames.
newtype Delta =
    Delta Vector3d
    deriving (Eq, Show)

-- | 'Delta' from given x, y and z length.
delta :: Length -> Length -> Length -> Delta
delta x y z = Delta (Vector3d (toMetres x) (toMetres y) (toMetres z))

-- | 'Delta' from given x, y and z length in __metres__.
deltaMetres :: Double -> Double -> Double -> Delta
deltaMetres x y z = delta (metres x) (metres y) (metres z)

-- | x component of given 'Delta'.
dx :: Delta -> Length
dx (Delta v) = metres (vx v)

-- | y component of given 'Delta'.
dy :: Delta -> Length
dy (Delta v) = metres (vy v)

-- | z component of given 'Delta'.
dz :: Delta -> Length
dz (Delta v) = metres (vz v)

-- | North, east and down delta (thus in frame 'FrameN').
newtype Ned =
    Ned Vector3d
    deriving (Eq, Show)

-- | 'Ned' from given north, east and down.
ned :: Length -> Length -> Length -> Ned
ned n e d = Ned (Vector3d (toMetres n) (toMetres e) (toMetres d))

-- | 'Ned' from given north, east and down in __metres__.
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

-- | @bearing v@ computes the bearing in compass angle of the NED vector @v@ from north.
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
--
bearing :: Ned -> Angle
bearing v =
    let a = atan2' (toMetres (east v)) (toMetres (north v))
     in normalise a (decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED vector @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: Ned -> Angle
elevation (Ned v) = negate' (asin' (vz v / vnorm v))

-- | @slantRange v@ computes the distance from origin in the local system of the NED vector @v@.
slantRange :: Ned -> Length
slantRange (Ned v) = metres (vnorm v)

-- | @deltaBetween p1 p2 f@ computes the exact 'Delta' between the two
-- positions @p1@ and @p2@ in frame @f@.
--
-- @
--     let p1 = latLongHeight 1 2 (metres (-3)) WGS84
--     let p2 = latLongHeight 4 5 (metres (-6)) WGS84
--     let w = decimalDegrees 5 -- wander azimuth
--     deltaBetween p1 p2 (frameL w)
--     -- Delta (Vector3d {vx = 359490.5782, vy = 302818.5225, vz = 17404.2714})
-- @
deltaBetween :: (Frame a, Model b) => Position b -> Position b -> (Position b -> a) -> Delta
deltaBetween p1 p2 f = deltaMetres (vx d) (vy d) (vz d)
  where
    e1 = evec p1
    e2 = evec p2
    de = vsub e2 e1
    -- rotation matrix to go from Earth Frame to Frame at p1
    rm = transpose (rEF (f p1))
    d = vrotate de rm

-- | @nedBetween p1 p2@ computes the exact 'Ned' vector between the two
-- positions @p1@ and @p2@, in north, east, and down.
--
-- Resulting 'Ned' delta is relative to @p1@: Due to the curvature of Earth and
-- different directions to the North Pole, the north, east, and down directions
-- will change (relative to Earth) for different places.
--
-- Position @p1@ must be outside the poles for the north and east directions to be defined.
--
-- @
--     let p1 = latLongHeightPos 1 2 (metres (-3)) WGS84
--     let p2 = latLongHeightPos 4 5 (metres (-6)) WGS84
--     nedBetween p1 p2
--     -- Ned (Vector3d {vx = 331730.2348, vy = 332997.875, vz = 17404.2714})
--     -- equivalent to:
--     deltaBetween p1 p2 frameN
--     -- Delta (Vector3d {vx = 331730.2348, vy = 332997.875, vz = 17404.2714})
-- @
nedBetween :: (Model a) => Position a -> Position a -> Ned
nedBetween p1 p2 = nedMetres (vx d) (vy d) (vz d)
  where
    (Delta d) = deltaBetween p1 p2 frameN

-- | @target p0 f d@ computes the target position from position @p0@ and delta @d@ in frame @f@.
--
-- @
--     let p0 = latLongHeightPos 49.66618 3.45063 zero WGS84
--     let y = decimalDegrees 10 -- yaw
--     let r = decimalDegrees 20 -- roll
--     let p = decimalDegrees 30 -- pitch
--     let d = deltaMetres 3000 2000 100
--     target p0 (frameB y r p) d
--     -- 49°41'30.486"N,3°28'52.561"E 6.0077m (WGS84)
--
-- @
target :: (Frame a, Model b) => Position b -> (Position b -> a) -> Delta -> Position b
target p0 f (Delta d) = ecefMetresPos x y z (model p0)
  where
    e0 = evec p0
    rm = rEF (f p0)
    c = vrotate d rm
    (Vector3d x y z) = vadd e0 c

-- | @targetN p0 d@ computes the target position from position @p0@ and north, east, down @d@.
--
-- @
--     let p0 = latLongHeightPos 49.66618 3.45063 zero WGS84
--     targetN p0 (nedMeters 100 200 300)
--     -- 49°40'1.485"N,3°27'12.242"E -299.9961m (WGS84)
--     -- equivalent to:
--     target p0 frameN (deltaMetres 100 200 300) wgs84
--     -- 49°40'1.485"N,3°27'12.242"E -299.9961m (WGS84)
-- @
targetN :: (Model a) => Position a -> Ned -> Position a
targetN p0 (Ned d) = target p0 frameN (Delta d)
