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
    , Error(..)
    , simple
    , circle
    , arc
    -- * calculations
    , contains
    , triangulate
    ) where

import Data.List (find)
import Data.Maybe (isJust, mapMaybe)

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import Data.Geo.Jord.Ellipsoid (meanRadius)
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.GreatCircle (MinorArc)
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Spherical, surface)
import Data.Geo.Jord.Triangle (Triangle)
import qualified Data.Geo.Jord.Triangle as Triangle

-- | A polygon whose vertices are horizontal geodetic positions.
data Polygon a =
    Polygon
        { vertices :: [HorizontalPosition a] -- ^ vertices of the polygon in __clockwise__ order.
        , edges :: [MinorArc a] -- ^ edges of the polyon in __clockwise__ order.
        , concave :: Bool -- ^ whether the polygon is concave.
        }
    deriving (Eq, Show)

-- | Error returned when attempting to create a polygon from invalid data. 
data Error
    = NotEnoughVertices -- ^ less than 3 vertices were supplied.
    | InvalidEdge -- ^ 2 consecutives vertices are antipodal or equal.
    | InvalidRadius -- ^ radius of circle or arc is <= 0.
    | EmptyArcRange -- ^ arc start angle == end angle.
    | SeflIntersectingEdge -- ^ 2 edges of the polygon intersect.
    deriving (Eq, Show)

-- | Simple polygon (outer ring only and not self-intersecting) from given vertices. Returns an error ('Left') if:
--
--     * less than 3 vertices are given.
--     * the given vertices defines self-intersecting edges.
--     * the given vertices contains duplicated positions or antipodal positions.
simple :: (Spherical a) => [HorizontalPosition a] -> Either Error (Polygon a)
simple vs
    | null vs = Left NotEnoughVertices
    | head vs == last vs = simple (init vs)
    | length vs < 3 = Left NotEnoughVertices
    | otherwise = simple' vs

-- | Circle from given centre and radius. The resulting polygon contains @nb@ vertices equally distant from one
-- another. Returns an error ('Left') if:
--
--     * given radius is 0
--     * given number of positions is less than 3
circle :: (Spherical a) => HorizontalPosition a -> Length -> Int -> Either Error (Polygon a)
circle c r nb
    | r <= Length.zero = Left InvalidRadius
    | nb < 3 = Left NotEnoughVertices
    | otherwise = Right (discretiseArc c r as)
  where
    n = fromIntegral nb :: Double
    as = take nb (iterate (\x -> x + pi * 2.0 / n) 0.0)

-- | Arc from given centre, radius and given start/end angles. The resulting polygon contains @nb@ vertices equally
-- distant from one another. Returns an error ('Left') if:
--
--     * given radius is 0
--     * given number of positions is less than 3
--     * difference between start and end angle is 0
arc :: (Spherical a)
    => HorizontalPosition a
    -> Length
    -> Angle
    -> Angle
    -> Int
    -> Either Error (Polygon a)
arc c r sa ea nb
    | r <= Length.zero = Left InvalidRadius
    | nb < 3 = Left NotEnoughVertices
    | range == Angle.zero = Left EmptyArcRange
    | otherwise = Right (discretiseArc c r as)
  where
    range = Angle.clockwiseDifference sa ea
    n = fromIntegral nb :: Double
    inc = Angle.toRadians range / (n - 1.0)
    r0 = Angle.toRadians sa
    as = take nb (iterate (+ inc) r0)

-- | @contains poly p@ returns 'True' if position @p@ is enclosed by the vertices of polygon
-- @poly@ - see 'GreatCircle.enclosedBy'.
contains :: (Spherical a) => Polygon a -> HorizontalPosition a -> Bool
contains poly p = GreatCircle.enclosedBy p (vertices poly)

-- | Triangulates the given polygon using the ear clipping method.
--
-- May return an empty list if the algorithm fails to find an ear (which probably indicates a bug in the implementation).
triangulate :: (Spherical a) => Polygon a -> [Triangle a]
triangulate p
    | length vs == 3 = [triangle vs]
    | otherwise = earClipping vs []
  where
    vs = vertices p

-- private
triangle :: (Spherical a) => [HorizontalPosition a] -> Triangle a
triangle vs = Triangle.unsafeMake (head vs) (vs !! 1) (vs !! 2)

earClipping :: (Spherical a) => [HorizontalPosition a] -> [Triangle a] -> [Triangle a]
earClipping vs ts
    | length vs == 3 = reverse (triangle vs : ts)
    | otherwise =
        case findEar vs of
            Nothing -> []
            (Just (p, e, n)) -> earClipping vs' ts'
                where ts' = Triangle.unsafeMake p e n : ts
                      vs' = filter (/= e) vs

findEar ::
       (Spherical a)
    => [HorizontalPosition a]
    -> Maybe (HorizontalPosition a, HorizontalPosition a, HorizontalPosition a)
findEar ps = find (`isEar` rs) convex
  where
    rs = reflices ps
    t3 = tuple3 ps
    convex = filter (\(_, v, _) -> v `notElem` rs) t3

-- | a convex vertex @c@ is an ear if triangle (prev, c, next) contains no reflex.
isEar ::
       (Spherical a)
    => (HorizontalPosition a, HorizontalPosition a, HorizontalPosition a)
    -> [HorizontalPosition a]
    -> Bool
isEar (p, c, n) = all (\r -> not (GreatCircle.enclosedBy r vs))
  where
    vs = [p, c, n]

-- | A reflex is a vertex where the polygon is concave.
-- a vertex is a reflex if previous vertex is left (assuming a clockwise polygon), otherwise it is a convex vertex
reflices :: (Spherical a) => [HorizontalPosition a] -> [HorizontalPosition a]
reflices ps = fmap (\(_, c, _) -> c) rs
  where
    t3 = tuple3 ps
    rs = filter (\(p, c, n) -> GreatCircle.side p c n == GreatCircle.LeftOf) t3

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

simple' :: (Spherical a) => [HorizontalPosition a] -> Either Error (Polygon a)
simple' vs
    | length es /= length vs = Left InvalidEdge
    | si = Left SeflIntersectingEdge
    | otherwise = Right (Polygon os es isConcave)
  where
    zs = tuple3 vs
    clockwise = sum (fmap (\(a, b, c) -> Angle.toRadians (GreatCircle.turn a b c)) zs) < 0.0
    os =
        if clockwise
            then vs
            else reverse vs
    es = mkEdges os
    zzs = tuple3 os
    isConcave =
        length vs > 3 && any (\(a, b, c) -> GreatCircle.side a b c == GreatCircle.LeftOf) zzs
    si = isConcave && selfIntersects es

tuple3 ::
       (Spherical a)
    => [HorizontalPosition a]
    -> [(HorizontalPosition a, HorizontalPosition a, HorizontalPosition a)]
tuple3 ps = zip3 l1 l2 l3
  where
    l1 = last ps : init ps
    l2 = ps
    l3 = tail ps ++ [head ps]

mkEdges :: (Spherical a) => [HorizontalPosition a] -> [MinorArc a]
mkEdges ps = mapMaybe (uncurry GreatCircle.minorArc) (zip ps (tail ps ++ [head ps]))

discretiseArc :: (Spherical a) => HorizontalPosition a -> Length -> [Double] -> Polygon a
discretiseArc c r as = Polygon ps (mkEdges ps) False
  where
    m = Geodetic.model c
    lat = Geodetic.latitude c
    lon = Geodetic.longitude c
    erm = Length.toMetres . meanRadius . surface $ m
    rm = erm * sin (Length.toMetres r / erm)
    z = sqrt (erm * erm - rm * rm)
    rya = pi / 2.0 - Angle.toRadians lat
    cy = cos rya
    sy = sin rya
    ry = [Math3d.vec3 cy 0 sy, Math3d.vec3 0 1 0, Math3d.vec3 (-sy) 0 cy]
    rza = Angle.toRadians lon
    cz = cos rza
    sz = sin rza
    rz = [Math3d.vec3 cz (-sz) 0, Math3d.vec3 sz cz 0, Math3d.vec3 0 0 1]
    anp = fmap (\a -> Math3d.vec3 ((-rm) * cos a) (rm * sin a) z) as -- arc at north pole
    rot = fmap (\v -> Math3d.multM (Math3d.multM v ry) rz) anp -- rotate each point to arc centre
    ps = fmap (`Geodetic.nvectorPos'` m) rot
