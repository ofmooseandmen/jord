-- |
-- Module:      Data.Geo.Jord.Triangle
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with triangles at the surface of a __spherical__ celestial body.
--
-- In order to use this module you should start with the following imports:
--
-- @
-- import qualified Data.Geo.Jord.Geodetic as Geodetic
-- import qualified Data.Geo.Jord.Triangle as Triangle
-- @
module Data.Geo.Jord.Triangle
    ( Triangle
    , vertex0
    , vertex1
    , vertex2
    , make
    , unsafeMake
    , centroid
    , circumcentre
    , contains
    ) where

import Control.Monad (join)
import Data.Maybe (fromJust)

import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model (Spherical)

-- | A triangle whose vertices are horizontal geodetic positions.
data Triangle a =
    Triangle (HorizontalPosition a) (HorizontalPosition a) (HorizontalPosition a)
    deriving (Eq, Show)

-- | First vertex of given triangle.
vertex0 :: (Spherical a) => Triangle a -> HorizontalPosition a
vertex0 (Triangle v _ _) = v

-- | Second vertex of given triangle.
vertex1 :: (Spherical a) => Triangle a -> HorizontalPosition a
vertex1 (Triangle _ v _) = v

-- | Third vertex of given triangle.
vertex2 :: (Spherical a) => Triangle a -> HorizontalPosition a
vertex2 (Triangle _ _ v) = v

-- | Triangle from given vertices. Returns 'Nothing' if some vertices are equal or some are antipodes of others.
make ::
       (Spherical a)
    => HorizontalPosition a
    -> HorizontalPosition a
    -> HorizontalPosition a
    -> Maybe (Triangle a)
make v0 v1 v2
    | v0 == v1 || v1 == v2 || v2 == v0 = Nothing
    | a0 == v1 || a0 == v2 || a1 == v2 = Nothing
    | otherwise = Just (Triangle v0 v1 v2)
  where
    a0 = Geodetic.antipode v0
    a1 = Geodetic.antipode v1

-- | Triangle from given vertices. This is unsafe, if any vertices are equal or some are antipodes of others,
-- the resulting triangle is actually undefined.
unsafeMake ::
       (Spherical a)
    => HorizontalPosition a
    -> HorizontalPosition a
    -> HorizontalPosition a
    -> Triangle a
unsafeMake = Triangle

-- | @contains t p@ returns 'True' if position @p@ is enclosed by the vertices of triangle
-- @t@ - see 'GreatCircle.enclosedBy'.
contains :: (Spherical a) => Triangle a -> HorizontalPosition a -> Bool
contains (Triangle v0 v1 v2) p = GreatCircle.enclosedBy p [v0, v1, v2]

-- | Computes the centroid of the given triangle: the position which is the intersection of the three medians of
-- the triangle (each median connecting a vertex with the midpoint of the opposite side).
--
-- The centroid is always within the triangle.
centroid :: (Spherical a) => Triangle a -> HorizontalPosition a
centroid (Triangle v0 v1 v2) = fromJust c -- this is safe unless triangle was created using unsafeMake.
  where
    m1 = GreatCircle.mean [v1, v2] >>= GreatCircle.minorArc v0
    m2 = GreatCircle.mean [v2, v0] >>= GreatCircle.minorArc v1
    c = join (GreatCircle.intersection <$> m1 <*> m2)

-- | The circumcentre of the triangle: the position which is equidistant from all three vertices.
--
-- The circumscribed circle or circumcircle of a triangle is a circle which passes through all
-- the vertices of the triangle; The circumcentre is not necessarily inside the triangle.
--
-- Thanks to STRIPACK: http://orion.math.iastate.edu/burkardt/f_src/stripack/stripack.f90
circumcentre :: (Spherical a) => Triangle a -> HorizontalPosition a
circumcentre (Triangle v0 v1 v2) = Geodetic.nvectorPos' cu (Geodetic.model v0)
  where
    e0 = Math3d.subtract (Geodetic.nvector v1) (Geodetic.nvector v0)
    e1 = Math3d.subtract (Geodetic.nvector v2) (Geodetic.nvector v0)
    cu = Math3d.cross e0 e1
