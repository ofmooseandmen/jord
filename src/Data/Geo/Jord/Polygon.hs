-- |
-- Module:      Data.Geo.Jord.Polygon
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- TODO
module Data.Geo.Jord.Polygon
    ( Polygon
    , vertices
    , edges
    , concave
    , simple
    , circle
    , arc
    , contains
    , triangulate
    ) where

import Data.Geo.Jord.Angle (Angle)
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.GreatCircle (MinorArc)
import Data.Geo.Jord.Length (Length)
import Data.Geo.Jord.Model (Spherical)
import Data.Geo.Jord.Triangle (Triangle)

data Polygon a =
    Polygon
        { vertices :: [HorizontalPosition a]
        , edges :: [MinorArc a]
        , concave :: Bool
        }

simple :: (Spherical a) => [HorizontalPosition a] -> Either String (Polygon a)
simple _ = Left "TODO"

circle :: (Spherical a) => HorizontalPosition a -> Length -> Int -> Either String (Polygon a)
circle c r nb = Left "TODO"

arc :: (Spherical a)
    => HorizontalPosition a
    -> Length
    -> Angle
    -> Angle
    -> Int
    -> Either String (Polygon a)
arc c r sa ea nb = Left "TODO"

contains :: (Spherical a) => Polygon a -> HorizontalPosition a -> Bool
contains _ _ = False

triangulate :: (Spherical a) => Polygon a -> [Triangle a]
triangulate _ = []
