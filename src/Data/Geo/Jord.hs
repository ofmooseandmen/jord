-- |
-- Module:      Data.Geo.Jord
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Convience module re-exporting all of Jord API while resolving function name clashes.
-- You'll probably rather want to import "Data.Geo.Jord.Position" and only the core module(s)
-- that suit your problem:
--
--    * "Data.Geo.Jord.LocalFrames"
--
--    * "Data.Geo.Jord.Geodesic"
--
--    * "Data.Geo.Jord.GreatCircle"
--
--    * "Data.Geo.Jord.Kinematics"
--
--    * "Data.Geo.Jord.Transformation"
--
module Data.Geo.Jord
    (
    -- * Core modules
      module Data.Geo.Jord.LocalFrames
    , module Data.Geo.Jord.Geodesic
    , module Data.Geo.Jord.GreatCircle
    , module Data.Geo.Jord.Kinematics
    , module Data.Geo.Jord.Position
    , module Data.Geo.Jord.Transformation
    -- * Aliases for name-clashing functions
    , destinationE
    , finalBearingE
    , initialBearingE
    , surfaceDistanceE
    , destinationS
    , finalBearingS
    , initialBearingS
    , surfaceDistanceS
    ) where

import Data.Geo.Jord.Geodesic hiding (destination, finalBearing, initialBearing, surfaceDistance)
import qualified Data.Geo.Jord.Geodesic as Geodesic
import Data.Geo.Jord.GreatCircle hiding (destination, finalBearing, initialBearing, surfaceDistance)
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import Data.Geo.Jord.Kinematics
import Data.Geo.Jord.LocalFrames
import Data.Geo.Jord.Position
import Data.Geo.Jord.Transformation

-- | alias for 'Geodesic.destination'.
destinationE :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a)
destinationE = Geodesic.destination

-- | alias for 'Geodesic.finalBearing'.
finalBearingE :: (Ellipsoidal a) => Position a -> Position a -> Maybe Angle
finalBearingE = Geodesic.finalBearing

-- | alias for 'Geodesic.initialBearing'.
initialBearingE :: (Ellipsoidal a) => Position a -> Position a -> Maybe Angle
initialBearingE = Geodesic.initialBearing

-- | alias for 'Geodesic.surfaceDistance'.
surfaceDistanceE :: (Ellipsoidal a) => Position a -> Position a -> Maybe Length
surfaceDistanceE = Geodesic.surfaceDistance

-- | alias for 'GreatCircle.destination'.
destinationS :: (Spherical a) => Position a -> Angle -> Length -> Position a
destinationS = GreatCircle.destination

-- | alias for 'GreatCircle.finalBearing'.
finalBearingS :: (Spherical a) => Position a -> Position a -> Maybe Angle
finalBearingS = GreatCircle.finalBearing

-- | alias for 'GreatCircle.initialBearing'.
initialBearingS :: (Spherical a) => Position a -> Position a -> Maybe Angle
initialBearingS = GreatCircle.initialBearing

-- | alias for 'GreatCircle.surfaceDistance'.
surfaceDistanceS :: (Spherical a) => Position a -> Position a -> Length
surfaceDistanceS = GreatCircle.surfaceDistance
