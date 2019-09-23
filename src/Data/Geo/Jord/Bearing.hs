-- |
-- Module:      Data.Geo.Jord.Bearing
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Functions to calculate bearings between positions for both ellispoidal and spherical models.
--
module Data.Geo.Jord.Bearing
    ( finalBearing
    , initialBearing
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Geodesic
import Data.Geo.Jord.Internal
import Data.Geo.Jord.Model
import Data.Geo.Jord.Position
import Data.Geo.Jord.Vector3d

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
--
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
--
-- The final bearing will differ from the 'initialBearing' by varying degrees according to distance and latitude.
--
-- Returns 'Nothing' if both positions are equals.
finalBearing :: (Model a) => Position a -> Position a -> Maybe Angle
finalBearing p1 p2
    | llEq p1 p2 = Nothing
    | otherwise =
        if isSphere' p1
            then Just (sFinalBearing p1 p2)
            else fmap snd (azimuths p1 p2 (surface . model $ p1))

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
--
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
--
-- Returns 'Nothing' if both positions are equals.
initialBearing :: (Model a) => Position a -> Position a -> Maybe Angle
initialBearing p1 p2
    | llEq p1 p2 = Nothing
    | otherwise =
        if isSphere' p1
            then Just (sInitialBearing p1 p2)
            else fmap fst (azimuths p1 p2 (surface . model $ p1))

sFinalBearing :: Position a -> Position a -> Angle
sFinalBearing p1 p2 = normalise (sInitialBearing p2 p1) (decimalDegrees 180)

sInitialBearing :: Position a -> Position a -> Angle
sInitialBearing p1 p2 = normalise a (decimalDegrees 360)
  where
    v1 = nvec p1
    v2 = nvec p2
    gc1 = vcross v1 v2 -- great circle through p1 & p2
    gc2 = vcross v1 nvNorthPole -- great circle through p1 & north pole
    a = radians (signedAngleRadians gc1 gc2 (Just v1))

isSphere' :: (Model a) => Position a -> Bool
isSphere' = isSphere . surface . model