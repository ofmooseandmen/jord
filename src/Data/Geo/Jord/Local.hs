-- |
-- Module:      Data.Geo.Jord.Local
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Type and functions for working with delta vectors in different local reference frames: all frames are location dependent.
--
-- In order to use this module you should start with the following imports:
--
-- @
-- import qualified Data.Geo.Jord.Geodetic as Geodetic
-- import qualified Data.Geo.Jord.Local as Local
-- @
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- Notes:
--
--     * The term Earth is used to be consistent with the paper. However any celestial body reference frame can be used.
--
--     * Though the API accept spherical models, doing so defeats the purpose of this module
--       which is to find exact solutions. Prefer using ellipsoidal models.
module Data.Geo.Jord.Local
    (
    -- * Local Reference frame
      Frame(..)
    -- * Body frame
    , FrameB
    , yaw
    , pitch
    , roll
    , bOrigin
    , frameB
    -- * Local level/wander azimuth frame
    , FrameL
    , wanderAzimuth
    , lOrigin
    , frameL
    -- * North-East-Down frame
    , FrameN
    , nOrigin
    , frameN
    -- * Deltas
    , Delta(..)
    , deltaMetres
    -- * Delta in the north, east, down frame
    , Ned(..)
    , nedMetres
    , bearing
    , elevation
    , slantRange
    -- * Calculations
    , deltaBetween
    , nedBetween
    , destination
    , destinationN
    ) where

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres, toMetres)
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Model)
import Data.Geo.Jord.Positions
import Data.Geo.Jord.Rotation

-- | class for local reference frames: a reference frame which is location dependant.
--
-- Supported frames:
--
--     * 'FrameB': 'rEF' returns R_EB
--
--     * 'FrameL': 'rEF' returns R_EL
--
--     * 'FrameN': 'rEF' returns R_EN
class Frame a where
    rEF :: a -> [Math3d.V3] -- ^ rotation matrix to transform vectors decomposed in frame @a@ to vectors decomposed Earth-Fixed frame.

-- | Body frame (typically of a vehicle).
--
--     * Position: The origin is in the vehicle’s reference point.
--
--     * Orientation: The x-axis points forward, the y-axis to the right (starboard) and the z-axis in the vehicle’s down direction.
--
--     * Comments: The frame is fixed to the vehicle.
data FrameB a =
    FrameB
        { yaw :: Angle -- ^ body yaw angle (vertical axis).
        , pitch :: Angle -- ^ body pitch angle (transverse axis).
        , roll :: Angle -- ^ body roll angle (longitudinal axis).
        , bOrigin :: Geodetic.Position a -- ^ frame origin.
        , bNorth :: Math3d.V3 -- ^ position of the north pole as /n/-vector.
        }
    deriving (Eq, Show)

-- | 'FrameB' from given yaw, pitch, roll, position (origin).
frameB :: (Model a) => Angle -> Angle -> Angle -> Geodetic.Position a -> FrameB a
frameB y p r o = FrameB y p r o (northPole o)

-- | R_EB: frame B to Earth
instance Frame (FrameB a) where
    rEF (FrameB y p r o np) = rm
      where
        rNB = zyx2r y p r
        n = FrameN o np
        rEN = rEF n
        rm = Math3d.dotM rEN rNB -- closest frames cancel: N

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
data FrameL a =
    FrameL
        { wanderAzimuth :: Angle -- ^ wander azimuth: angle between x-axis of the frame L and the north direction.
        , lOrigin :: Geodetic.Position a -- ^ frame origin.
        }
    deriving (Eq, Show)

-- | R_EL: frame L to Earth
instance Frame (FrameL m) where
    rEF (FrameL w o) = rm
      where
        lat = Geodetic.latitude o
        lon = Geodetic.longitude o
        r = xyz2r lon (Angle.negate lat) w
        rEe' = [Math3d.vec3 0 0 (-1), Math3d.vec3 0 1 0, Math3d.vec3 1 0 0]
        rm = Math3d.dotM rEe' r

-- | 'FrameL' from given wander azimuth, position (origin).
frameL :: (Model a) => Angle -> Geodetic.Position a -> FrameL a
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
data FrameN a =
    FrameN
        { nOrigin :: Geodetic.Position a -- ^ frame origin.
        , nNorth :: Math3d.V3 -- ^ position of the north pole as /n/-vector.
        }
    deriving (Eq, Show)

-- | R_EN: frame N to Earth
instance Frame (FrameN a) where
    rEF (FrameN o np) = Math3d.transposeM rm
      where
        vo = Geodetic.nvector o
        rd = Math3d.scale vo (-1.0) -- down (pointing opposite to n-vector)
        re = Math3d.unit (Math3d.cross np vo) -- east (pointing perpendicular to the plane)
        rn = Math3d.cross re rd -- north (by right hand rule)
        rm = [rn, re, rd]

-- | 'FrameN' from given position (origin).
frameN :: (Model a) => Geodetic.Position a -> FrameN a
frameN p = FrameN p (northPole p)

-- | delta between position in one of the reference frames.
data Delta =
    Delta
        { dx :: Length -- ^ x component.
        , dy :: Length -- ^ y component.
        , dz :: Length -- ^ z component.
        }
    deriving (Eq, Show)

-- | 'Delta' from given x, y and z length in __metres__.
deltaMetres :: Double -> Double -> Double -> Delta
deltaMetres x y z = Delta (Length.metres x) (Length.metres y) (Length.metres z)

-- | North, east and down delta (thus in frame 'FrameN').
data Ned =
    Ned
        { north :: Length -- ^ North component.
        , east :: Length -- ^ East component.
        , down :: Length -- ^ Down component.
        }
    deriving (Eq, Show)

-- | 'Ned' from given north, east and down in __metres__.
nedMetres :: Double -> Double -> Double -> Ned
nedMetres n e d = Ned (Length.metres n) (Length.metres e) (Length.metres d)

-- | @bearing v@ computes the bearing in compass angle of the NED vector @v@ from north.
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
bearing :: Ned -> Angle
bearing (Ned n e _) =
    let a = Angle.atan2 (Length.toMetres e) (Length.toMetres n)
     in Angle.normalise a (Angle.decimalDegrees 360.0)

-- | @elevation v@ computes the elevation of the NED vector @v@ from horizontal (ie tangent to ellipsoid surface).
elevation :: Ned -> Angle
elevation n = Angle.negate (Angle.asin (Math3d.v3z v / Math3d.norm v))
  where
    v = nedV3 n

-- | @slantRange v@ computes the distance from origin in the local system of the NED vector @v@.
slantRange :: Ned -> Length
slantRange = Length.metres . Math3d.norm . nedV3

-- | @deltaBetween p1 p2 f@ computes the exact 'Delta' between the two
-- positions @p1@ and @p2@ in local frame @f@. For example:
--
-- >>> let p1 = Geodetic.latLongHeightPos 1 2 (Length.metres (-3)) WGS84
-- >>> let p2 = Geodetic.latLongHeightPos 4 5 (Length.metres (-6)) WGS84
-- >>> let w = Angle.decimalDegrees 5 -- wander azimuth
-- >>> Local.deltaBetween p1 p2 (Local.frameL w)
-- Delta {dx = 359.490578214km, dy = 302.818522536km, dz = 17.404271362km}
deltaBetween ::
       (Frame a, Model b)
    => Geodetic.Position b
    -> Geodetic.Position b
    -> (Geodetic.Position b -> a)
    -> Delta
deltaBetween p1 p2 f = deltaMetres (Math3d.v3x d) (Math3d.v3y d) (Math3d.v3z d)
  where
    g1 = Geocentric.metresCoords . toGeocentric $ p1
    g2 = Geocentric.metresCoords . toGeocentric $ p2
    de = Math3d.subtract g2 g1
    -- rotation matrix to go from Earth Frame to Frame at p1
    rm = Math3d.transposeM (rEF (f p1))
    d = Math3d.multM de rm

-- | @nedBetween p1 p2@ computes the exact 'Ned' vector between the two
-- positions @p1@ and @p2@, in north, east, and down. For example:
--
-- >>> let p1 = Geodetic.latLongHeightPos 1 2 (Length.metres (-3)) WGS84
-- >>> let p2 = Geodetic.latLongHeightPos 4 5 (Length.metres (-6)) WGS84
-- >>> Local.nedBetween p1 p2
-- Ned {north = 331.730234781km, east = 332.997874989km, down = 17.404271362km}
--
-- Resulting 'Ned' delta is relative to @p1@: Due to the curvature of Earth and
-- different directions to the North Pole, the north, east, and down directions
-- will change (relative to Earth) for different places.
--
-- Position @p1@ must be outside the poles for the north and east directions to be defined.
--
-- This is equivalent to:
--
-- > Local.deltaBetween p1 p2 Local.frameN
nedBetween :: (Model a) => Geodetic.Position a -> Geodetic.Position a -> Ned
nedBetween p1 p2 = Ned n e d
  where
    (Delta n e d) = deltaBetween p1 p2 frameN

-- | @destination p0 f d@ computes the destination position from position @p0@ and delta @d@ in local frame @f@. For
-- example:
--
-- >>> let p0 = Geodetic.latLongHeightPos 49.66618 3.45063 Length.zero WGS84
-- >>> let y = Angle.decimalDegrees 10 -- yaw
-- >>> let r = Angle.decimalDegrees 20 -- roll
-- >>> let p = Angle.decimalDegrees 30 -- pitch
-- >>> let d = Local.deltaMetres 3000 2000 100
-- >>> Local.destination p0 (Local.frameB y r p) d
-- 49°41'30.485"N,3°28'52.561"E 6.007735m (WGS84)
destination ::
       (Frame a, Model b)
    => Geodetic.Position b
    -> (Geodetic.Position b -> a)
    -> Delta
    -> Geodetic.Position b
destination p0 f d = toGeodetic gt
  where
    g0 = Geocentric.metresCoords . toGeocentric $ p0
    rm = rEF (f p0)
    c = Math3d.multM (deltaV3 d) rm
    v = Math3d.add g0 c
    gt = Geocentric.metresPos' v (Geodetic.model' p0)

-- | @destinationN p0 d@ computes the destination position from position @p0@ and north, east, down @d@. For example:
--
-- >>> let p0 = Geodetic.latLongHeightPos 49.66618 3.45063 Length.zero WGS84
-- >>> Local.destinationN p0 (Local.nedMetres 100 200 300)
-- 49°40'1.484"N,3°27'12.242"E -299.996086m (WGS84)
-- This is equivalent to:
--
-- > Local.destination p0 Local.frameN
destinationN :: (Model a) => Geodetic.Position a -> Ned -> Geodetic.Position a
destinationN p0 (Ned n e d) = destination p0 frameN (Delta n e d)

nedV3 :: Ned -> Math3d.V3
nedV3 (Ned n e d) = Math3d.vec3 (Length.toMetres n) (Length.toMetres e) (Length.toMetres d)

deltaV3 :: Delta -> Math3d.V3
deltaV3 (Delta x' y' z') =
    Math3d.vec3 (Length.toMetres x') (Length.toMetres y') (Length.toMetres z')

northPole :: (Model a) => Geodetic.Position a -> Math3d.V3
northPole = Geodetic.nvector . Geodetic.northPole . Geodetic.model'
