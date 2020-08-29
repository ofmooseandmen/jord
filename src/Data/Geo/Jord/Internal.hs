-- |
-- Module:      Data.Geo.Jord.Internal
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- internal functions.
--
module Data.Geo.Jord.Internal
    ( angleBetween
    , signedAngleBetween
    , llEq
    ) where

import Data.Geo.Jord.Position

-- | angle between 2 vectors.
angleBetween :: Vector3d -> Vector3d -> Angle
angleBetween v1 v2 = signedAngleBetween v1 v2 Nothing

-- | Signed angle between 2 vectors.
-- If @n@ is 'Nothing', the angle is always in [0..pi], otherwise it is in [-pi, +pi],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
signedAngleBetween :: Vector3d -> Vector3d -> Maybe Vector3d -> Angle
signedAngleBetween v1 v2 n = radians (atan2 sinO cosO)
  where
    sign = maybe 1 (signum . vdot (vcross v1 v2)) n
    sinO = sign * vnorm (vcross v1 v2)
    cosO = vdot v1 v2

-- | both position have same latitude and longitude irrespective of model ?
llEq :: Position a -> Position a -> Bool
llEq p1 p2 = latitude p1 == latitude p2 && longitude p1 == longitude p2
