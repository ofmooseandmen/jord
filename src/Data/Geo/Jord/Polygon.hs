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

-- | A polygon whose vertices are horizontal geodetic positions.
data Polygon a =
    Polygon
        { vertices :: [HorizontalPosition a] -- ^ vertices of the polygon in __clockwise__ order.
        , edges :: [MinorArc a] -- ^ edges of the polyon in __clockwise__ order.
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

-- | Circle from given centre and radius. The resulting polygon contains @nb@ vertices equally distant from one
-- another. Returns an error ('Left') if:
--
--     * given radius is 0
--     * given number of positions is less than 3
circle :: (Spherical a) => HorizontalPosition a -> Length -> Int -> Either String (Polygon a)
circle c r nb
    | r <= Length.zero = Left "invalid radius"
    | nb < 3 = Left "invalid number of positions"
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
    -> Either String (Polygon a)
arc c r sa ea nb
    | r <= Length.zero = Left "invalid radius"
    | nb < 3 = Left "invalid number of positions"
    | range == Angle.zero = Left "invalid range"
    | otherwise = Right (discretiseArc c r as)
  where
    range = Angle.clockwiseDifference sa ea
    n = fromIntegral nb :: Double
    inc = (Angle.toRadians range) / (n - 1.0)
    r0 = Angle.toRadians sa
    as = take nb (iterate (\x -> x + inc) r0)

-- | @contains poly p@ returns 'True' if position @p@ is enclosed by the vertices of polygon
-- @poly@ - see 'GreatCircle.enclosedBy'.
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
    l3 = tail ps ++ [head ps] ++ [head ps]

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
    rya = pi / 2.0 - (Angle.toRadians lat)
    cy = cos rya
    sy = sin rya
    ry = [Math3d.vec3 cy 0 sy, Math3d.vec3 0 1 0, Math3d.vec3 (-sy) 0 cy]
    rza = Angle.toRadians lon
    cz = cos rza
    sz = sin rza
    rz = [Math3d.vec3 cz (-sz) 0, Math3d.vec3 sz cz 0, Math3d.vec3 0 0 1]
    anp = fmap (\a -> Math3d.vec3 ((-rm) * cos a) (rm * sin a) z) as -- arc at north pole
    rot = fmap (\v -> Math3d.multM (Math3d.multM v ry) rz) anp -- rotate each point to arc centre
    ps = fmap (\v -> Geodetic.nvectorPos' v m) rot
