-- |
-- Module:      Data.Geo.Jord.Internal
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- internal functions.
--
module Data.Geo.Jord.Internal
    ( angleRadians
    , signedAngleRadians
    , nvNorthPole
    , nvSouthPole
    ) where

import Data.Geo.Jord.Vector3d

-- | angle in __radians__ between 2 vectors.
angleRadians :: Vector3d -> Vector3d -> Double
angleRadians v1 v2 = signedAngleRadians v1 v2 Nothing

-- | Signed angle in __radians__ between 2 vectors.
-- If @n@ is 'Nothing', the angle is always in [0..pi], otherwise it is in [-pi, +pi],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
signedAngleRadians :: Vector3d -> Vector3d -> Maybe Vector3d -> Double
signedAngleRadians v1 v2 n = atan2 sinO cosO
  where
    sign = maybe 1 (signum . vdot (vcross v1 v2)) n
    sinO = sign * vnorm (vcross v1 v2)
    cosO = vdot v1 v2

-- | Horizontal position of the North Pole (/n/-vector).
nvNorthPole :: Vector3d
nvNorthPole = Vector3d 0.0 0.0 1.0

-- | Horizontal position of the South Pole (/n/-vector).
nvSouthPole :: Vector3d
nvSouthPole = Vector3d 0.0 0.0 (-1.0)