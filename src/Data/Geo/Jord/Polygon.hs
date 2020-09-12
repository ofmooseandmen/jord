-- |
-- Module:      Data.Geo.Jord.Polygon
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with polygons at the surface of a __spherical__ celestial body.
--
-- In order to use this module you should start with the following imports:
--
-- @
-- import qualified Data.Geo.Jord.Geodetic as Geodetic
-- import qualified Data.Geo.Jord.Polygon as Polygon
-- @
module Data.Geo.Jord.Polygon
    (
    -- * The 'Polygon' type
      Polygon
    , vertices
    , edges
    , concave
    -- * smart constructors
    , simple
    , circle
    , arc
    -- * calculations
    , contains
    , triangulate
    ) where

import Data.Maybe (isJust, mapMaybe)

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.GreatCircle (MinorArc)
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import Data.Geo.Jord.Length (Length)
import Data.Geo.Jord.Model (Spherical)
import Data.Geo.Jord.Triangle (Triangle)

-- | A polygon whose vertices are horizontal geodetic positions.
data Polygon a =
    Polygon
        { vertices :: [HorizontalPosition a] -- ^ the vertices of the polygon.
        , edges :: [MinorArc a] -- ^ the edges of the polyon.
        , concave :: Bool -- ^ whether the polygon is concave.
        }
    deriving (Eq, Show)

-- | Simple polygon (outer ring only and not self-intersecting) from given vertices. Returns an error ('Left') if:
--
--     * less than 3 vertices are given.
--     * the given vertices defines self-intersecting edges.
--     * the given vertices contains duplicated positions or antipodal positions.
simple :: (Spherical a) => [HorizontalPosition a] -> Either String (Polygon a)
simple vs
    | null vs = Left "no vertex"
    | head vs == last vs = simple (init vs)
    | length vs < 3 = Left "not enough vertices"
    -- FIXME: no equal/antipodal positions
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
-- | [mAB, mBC, mCD, mDE, mEA]
-- no intersections:
-- mAB vs [mCD, mDE]
-- mBC vs [mDE, mEA]
-- mCD vs [mEA]
selfIntersects :: (Spherical a) => [MinorArc a] -> Bool
selfIntersects ps
    | length ps < 4 = False
    | otherwise = any intersects pairs
  where
    (_, pairs) = makePairs' (ps, [])

intersects :: (Spherical a) => (MinorArc a, [MinorArc a]) -> Bool
intersects (ma, mas) = any (isJust . GreatCircle.intersection ma) mas

makePairs' ::
       (Spherical a)
    => ([MinorArc a], [(MinorArc a, [MinorArc a])])
    -> ([MinorArc a], [(MinorArc a, [MinorArc a])])
makePairs' (xs, ps)
    | length xs < 3 = (xs, ps)
    | otherwise = makePairs' (nxs, np : ps)
  where
    nxs = tail xs
    -- if ps is empty (first call), drop last minor arc as it connect to first minor arc
    versus =
        if null ps
            then init . tail . tail $ xs
            else tail . tail $ xs
    np = (head xs, versus)

simple' :: (Spherical a) => [HorizontalPosition a] -> Either String (Polygon a)
simple' vs =
    if si
        then Left "self-intersecting edges"
        else Right (Polygon os es isConcave)
  where
    zs = zip3' vs
    clockwise = sum (fmap (\(a, b, c) -> Angle.toRadians (GreatCircle.turn a b c)) zs) < 0.0
    os =
        if clockwise
            then vs
            else reverse vs
    es = mkEdges os
    zzs = zip3' os
    isConcave =
        length vs > 3 && any (\(a, b, c) -> GreatCircle.side a b c == GreatCircle.LeftOf) zzs
    si = isConcave && selfIntersects es

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
