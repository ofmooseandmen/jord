-- |
-- Module:      Data.Geo.Jord.Internal
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- internal functions.
--
module Data.Geo.Jord.Internal
    ( ad
    , nvec
    , sad
    ) where

import Data.Geo.Jord.AngularPosition (pos)
import Data.Geo.Jord.Transformation (NTransform(..))
import Data.Geo.Jord.Vector3d

-- | angle in  __radians__ between 2 /n/-vectors (as 'Vector3d').
ad :: Vector3d -> Vector3d -> Double
ad v1 v2 = sad v1 v2 Nothing

-- | /n/-vector (as a 'Vector3d') from given position.
nvec :: (NTransform a) => a -> Vector3d
nvec = vec . pos . toNVector

-- | Signed angle in __radians__ between 2 /n/-vectors (as 'Vector3d').
-- If @n@ is 'Nothing', the angle is always in [0..pi], otherwise it is in [-pi, +pi],
-- signed + if @v1@ is clockwise looking along @n@, - in opposite direction.
sad :: Vector3d -> Vector3d -> Maybe Vector3d -> Double
sad v1 v2 n = atan2 sinO cosO
  where
    sign = maybe 1 (signum . vdot (vcross v1 v2)) n
    sinO = sign * vnorm (vcross v1 v2)
    cosO = vdot v1 v2
