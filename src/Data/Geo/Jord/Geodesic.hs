module Data.Geo.Jord.Geodesic
    ( destination
    , direct
    , inverse
    , finalBearing
    , initialBearing
    , surfaceDistance
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Bodies
import Data.Geo.Jord.Length
import Data.Geo.Jord.Position
import qualified Data.Geo.Jord.Spherical as S

-- TODO: examples (spherical and ellipsoidal)
direct :: (Model a) => Position a -> Angle -> Length -> Maybe (Position a, Angle)
direct _ _ _ = Nothing

inverse :: (Model a) => Position a -> Position a -> Maybe (Length, Angle, Angle)
inverse _ _ = Nothing

-- | @destination p b d@ computes the position reached from position @p@ having
-- travelled the distance @d@ on the initial bearing (compass angle) @b@.
-- Note that the  bearing will normally vary before destination is reached.
--
destination :: (Model a) => Position a -> Angle -> Length -> Position a
destination p b d =
    case shape m of
        (Sphere r) -> nvh (S.destination p b d r) h m
        (Ellipsoid _) -> error "TODO ellispoidal"
  where
    h = height p
    m = model p

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
--
-- The final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
--
-- Returns 'Nothing' if both positions are equals.
finalBearing :: (Model a) => Position a -> Position a -> Maybe Angle
finalBearing p1 p2 =
    case shape . model $ p1 of
        (Sphere _) -> S.finalBearing p1 p2
        (Ellipsoid _) -> Nothing -- TODO ellispoidal

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
--
-- Returns 'Nothing' if both positions are equals.
initialBearing :: (Model a) => Position a -> Position a -> Maybe Angle
initialBearing p1 p2 =
    case shape . model $ p1 of
        (Sphere _) -> S.initialBearing p1 p2
        (Ellipsoid _) -> error "TODO ellispoidal"

-- | @surfaceDistance p1 p2@ computes the length of geodesic between the positions @p1@ and @p2@.
surfaceDistance :: (Model a) => Position a -> Position a -> Length
surfaceDistance p1 p2 =
    case shape . model $ p1 of
        (Sphere r) -> S.surfaceDistance p1 p2 r
        (Ellipsoid _) -> error "TODO ellispoidal"