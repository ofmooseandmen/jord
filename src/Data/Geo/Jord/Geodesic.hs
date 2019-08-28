module Data.Geo.Jord.Geodesic
    ( Geodesic
    , geodesicStart
    , geodesicEnd
    , geodesicLength
    , geodesicInitialBearing
    , geodesicFinalBearing
    , direct
    , inverse
    , antipode
    , destination
    , finalBearing
    , initialBearing
    , surfaceDistance
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Bodies
import Data.Geo.Jord.Internal
import Data.Geo.Jord.Length
import Data.Geo.Jord.Position
import Data.Geo.Jord.Vector3d

data Geodesic a =
    Geodesic
        { geodesicStart :: Position a
        , geodesicEnd :: Position a
        , geodesicLength :: Length
        , geodesicInitialBearing :: Angle
        , geodesicFinalBearing :: Angle
        }
    deriving (Eq, Show)

direct :: (Model a) => Position a -> Angle -> Length -> Maybe (Geodesic a)
direct p b d = fmap (Geodesic p e d b) fb
  where
    e = destination p b d
    fb = finalBearing p e

inverse :: (Model a) => Position a -> Position a -> Maybe (Geodesic a)
inverse p1 p2 =
    pure (Geodesic p1 p2) <*> surfaceDistance p1 p2 <*> initialBearing p1 p2 <*> finalBearing p1 p2

-- | @antipode p@ computes the antipodal position of @p@: the position which is
-- diametrically opposite to @p@.
antipode :: (Model a) => Position a -> Position a
antipode p = nvh nv h (model p)
  where
    h = height p
    nv = vscale (nvec p) (-1.0)

-- | @destination p b d@ computes the position reached from position @p@ having
-- travelled the distance @d@ on the initial bearing (compass angle) @b@.
-- Note that the  bearing will normally vary before destination is reached.
--
-- TODO: examples (spherical and ellipsoidal)
destination :: (Model a) => Position a -> Angle -> Length -> Position a
destination p b d
    | toMetres d == 0.0 = p
    | otherwise = destination' p b d

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
--
-- The final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
--
-- Returns 'Nothing' if both positions are equals. TODO: what about vincenty?
finalBearing :: (Model a) => Position a -> Position a -> Maybe Angle
finalBearing p1 p2
    | p1 == p2 = Nothing
    | otherwise = finalBearing' p1 p2

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
--
-- Compass angles are clockwise angles from true north: 0 = north, 90 = east, 180 = south, 270 = west.
--
-- Returns 'Nothing' if both positions are equals. TODO: what about vincenty?
initialBearing :: (Model a) => Position a -> Position a -> Maybe Angle
initialBearing p1 p2
    | p1 == p2 = Nothing
    | otherwise = initialBearing' p1 p2

-- | @surfaceDistance p1 p2@ computes the surface distance (length of geodesic) between the positions @p1@ and @p2@.
--
-- Note: For spherical model the length if always defined.
--
-- TODO: add greatCircleDistance for Spherical model only?
surfaceDistance :: (Model a) => Position a -> Position a -> Maybe Length
surfaceDistance p1 p2 =
    case shape m of
        (Sphere r) ->
            let a = radians (angleRadians (nvec p1) (nvec p2))
             in Just (arcLength a r)
        (Ellipsoid _) -> Nothing -- TODO Vincenty
  where
    m = model p1

-- | private
destination' :: (Model a) => Position a -> Angle -> Length -> Position a
destination' p b d =
    case shape m of
        (Sphere r) -> nvh (sDestination p b d r) h m
        (Ellipsoid _) -> error "TODO Vincenty"
  where
    h = height p
    m = model p

sDestination :: Position a -> Angle -> Length -> Length -> Vector3d
sDestination p b d r = vadd (vscale nv (cos' ta)) (vscale de (sin' ta))
  where
    nv = nvec p
    ed = vunit (vcross nvNorthPole nv) -- east direction vector at v
    nd = vcross nv ed -- north direction vector at v
    ta = central d r -- central angle
    de = vadd (vscale nd (cos' b)) (vscale ed (sin' b)) -- vunit vector in the direction of the azimuth

initialBearing' :: (Model a) => Position a -> Position a -> Maybe Angle
initialBearing' p1 p2 =
    case shape m of
        (Sphere _) -> Just (sInitialBearing p1 p2)
        (Ellipsoid _) -> Nothing -- TODO Vincenty
  where
    m = model p1

sInitialBearing :: Position a -> Position a -> Angle
sInitialBearing p1 p2 = normalise a (decimalDegrees 360)
  where
    v1 = nvec p1
    v2 = nvec p2
    gc1 = vcross v1 v2 -- great circle through p1 & p2
    gc2 = vcross v1 nvNorthPole -- great circle through p1 & north pole
    a = radians (signedAngleRadians gc1 gc2 (Just v1))

finalBearing' :: (Model a) => Position a -> Position a -> Maybe Angle
finalBearing' p1 p2 =
    case shape m of
        (Sphere _) -> Just (sFinalBearing p1 p2)
        (Ellipsoid _) -> Nothing -- TODO Vincenty
  where
    m = model p1

sFinalBearing :: Position a -> Position a -> Angle
sFinalBearing p1 p2 = normalise b (decimalDegrees 180)
  where
    b = sInitialBearing p2 p1