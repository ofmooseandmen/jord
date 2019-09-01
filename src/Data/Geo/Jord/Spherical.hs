-- |
-- Module:      Data.Geo.Jord.Spherical
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- internal functions for calculations on spherical models.
--
module Data.Geo.Jord.Spherical
    ( angleRadians
    , destination
    , finalBearing
    , initialBearing
    , signedAngleRadians
    , surfaceDistance
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Length
import Data.Geo.Jord.Position
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Vector3d

-- | angle in __radians__ between 2 vectors.
angleRadians :: Vector3d -> Vector3d -> Double
angleRadians v1 v2 = signedAngleRadians v1 v2 Nothing

destination :: Position a -> Angle -> Length -> Length -> Vector3d
destination p b d r
    | d == zero = nv
    | otherwise = vadd (vscale nv (cos' ta)) (vscale de (sin' ta))
  where
    nv = nvec p
    ed = vunit (vcross nvNorthPole nv) -- east direction vector at v
    nd = vcross nv ed -- north direction vector at v
    ta = central d r -- central angle
    de = vadd (vscale nd (cos' b)) (vscale ed (sin' b)) -- vunit vector in the direction of the azimuth

finalBearing :: Position a -> Position a -> Maybe Angle
finalBearing p1 p2
    | eq p1 p2 = Nothing
    | otherwise = fmap (`normalise` decimalDegrees 180) b
  where
    b = initialBearing p2 p1

initialBearing :: Position a -> Position a -> Maybe Angle
initialBearing p1 p2
    | eq p1 p2 = Nothing
    | otherwise = Just (normalise a (decimalDegrees 360))
  where
    v1 = nvec p1
    v2 = nvec p2
    gc1 = vcross v1 v2 -- great circle through p1 & p2
    gc2 = vcross v1 nvNorthPole -- great circle through p1 & north pole
    a = radians (signedAngleRadians gc1 gc2 (Just v1))

-- | Signed angle in __radians__ between 2 vectors.
-- If @n@ is 'Nothing', the angle is always in [0..pi], otherwise it is in [-pi, +pi],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
signedAngleRadians :: Vector3d -> Vector3d -> Maybe Vector3d -> Double
signedAngleRadians v1 v2 n = atan2 sinO cosO
  where
    sign = maybe 1 (signum . vdot (vcross v1 v2)) n
    sinO = sign * vnorm (vcross v1 v2)
    cosO = vdot v1 v2

surfaceDistance :: Position a -> Position a -> Length -> Length
surfaceDistance p1 p2 = arcLength a
  where
    a = radians (angleRadians (nvec p1) (nvec p2))

-- | both position have same latitude and longitude ?
eq :: Position a -> Position a -> Bool
eq p1 p2 = latitude p1 == latitude p2 && longitude p1 == longitude p2