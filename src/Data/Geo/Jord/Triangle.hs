-- |
-- Module:      Data.Geo.Jord.Triangle
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- TODO
module Data.Geo.Jord.Triangle
    ( Triangle
    , vertex1
    , vertex2
    , vertex3
    , edge1
    , edge2
    , edge3
    , centroid
    , circumcentre
    , make
    , contains
    , circumcircleContains
    ) where

import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.GreatCircle (MinorArc)
import Data.Geo.Jord.Model (Spherical)

data Triangle a =
    Triangle
        { vertex1 :: HorizontalPosition a
        , vertex2 :: HorizontalPosition a
        , vertex3 :: HorizontalPosition a
        , edge1 :: MinorArc a
        , edge2 :: MinorArc a
        , edge3 :: MinorArc a
        , centroid :: HorizontalPosition a
        , circumcentre :: HorizontalPosition a
        }
    deriving (Eq, Show)

make ::
       (Spherical a)
    => HorizontalPosition a
    -> HorizontalPosition a
    -> HorizontalPosition a
    -> Either String (Triangle a)
make _ _ _ = Left "TODO"

contains :: (Spherical a) => Triangle a -> HorizontalPosition a -> Bool
contains _ _ = False

circumcircleContains :: (Spherical a) => Triangle a -> HorizontalPosition a -> Bool
circumcircleContains _ _ = False
