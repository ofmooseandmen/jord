-- |
-- Module:      Data.Geo.Jord
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Convience module re-exporting all of Jord API while resolving function name clashes.
--
module Data.Geo.Jord
    ( module Data.Geo.Jord.LocalFrames
    , module Data.Geo.Jord.Geodesic
    , module Data.Geo.Jord.GreatCircle
    , module Data.Geo.Jord.Kinematics
    , module Data.Geo.Jord.Position
    , module Data.Geo.Jord.Transformation
    , module Data.Geo.Jord.Transformations
    , destinationE
    , destinationS
    , finalBearingE
    , finalBearingS
    , initialBearingE
    , initialBearingS
    , surfaceDistanceE
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
import Data.Geo.Jord.Transformations

destinationE :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a)
destinationE = Geodesic.destination

destinationS :: (Spherical a) => Position a -> Angle -> Length -> Position a
destinationS = GreatCircle.destination

finalBearingE :: (Ellipsoidal a) => Position a -> Position a -> Maybe Angle
finalBearingE = Geodesic.finalBearing

finalBearingS :: (Spherical a) => Position a -> Position a -> Maybe Angle
finalBearingS = GreatCircle.finalBearing

initialBearingE :: (Ellipsoidal a) => Position a -> Position a -> Maybe Angle
initialBearingE = Geodesic.initialBearing

initialBearingS :: (Spherical a) => Position a -> Position a -> Maybe Angle
initialBearingS = GreatCircle.initialBearing

surfaceDistanceE :: (Ellipsoidal a) => Position a -> Position a -> Maybe Length
surfaceDistanceE = Geodesic.surfaceDistance

surfaceDistanceS :: (Spherical a) => Position a -> Position a -> Length
surfaceDistanceS = GreatCircle.surfaceDistance
