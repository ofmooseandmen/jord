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

import Data.Maybe (mapMaybe)

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.GreatCircle (MinorArc)
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
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
simple vs
    | null vs = Left "no vertex"
    | head vs == last vs = simple (init vs)
    | length vs < 3 = Left "not enough vertices"
-- FIXME: check needed: as always no equal/antipodal positions
-- FIXME: check needed: self-intersecting
    | otherwise = simple' vs

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
contains poly p = GreatCircle.enclosedBy p (vertices poly)

triangulate :: (Spherical a) => Polygon a -> [Triangle a]
triangulate _ = []

-- private
simple' :: (Spherical a) => [HorizontalPosition a] -> Either String (Polygon a)
simple' vs = Right (Polygon os es isConcave)
  where
    zs = zip3' vs
    clockwise = sum (fmap (\(a, b, c) -> Angle.toRadians (GreatCircle.turn a b c)) zs) < 0.0
    os =
        if clockwise
            then vs
            else reverse vs
    es = mkEdges os
    zzs =
        if clockwise
            then zs
            else reverse zs
    isConcave =
        length vs > 4 && any (\(a, b, c) -> GreatCircle.side a b c == GreatCircle.LeftOf) zzs

zip3' ::
       (Spherical a)
    => [HorizontalPosition a]
    -> [(HorizontalPosition a, HorizontalPosition a, HorizontalPosition a)]
zip3' ps = zip3 l1 l2 l3
  where
    l1 = last ps : init ps
    l2 = ps
    l3 = tail ps ++ [head ps] ++ [head ps]

mkEdges :: (Spherical a) => [HorizontalPosition a] -> [MinorArc a]
mkEdges ps = mapMaybe (uncurry GreatCircle.minorArc) (zip ps (tail ps ++ [head ps]))
