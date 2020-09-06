-- |
-- Module:      Data.Geo.Jord.Geocentric
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geocentric coordinates of points (X, Y, and Z Cartesian coordinates) in specified models.
--
-- For the Earth the coordinate system is known as ECEF (acronym for earth-centered, earth-fixed),
-- or ECR (initialism for earth-centered rotational).
--
-- In order to use this module you should start with the following imports:
--
-- @
-- import qualified Data.Geo.Jord.Geocentric as Geocentric
-- import qualified Data.Geo.Jord.Length as Length
-- import Data.Geo.Jord.Models
-- @
--
-- see "Data.Geo.Jord.Models" for supported models.
module Data.Geo.Jord.Geocentric
    ( Position(..)
    , coords
    , metresCoords
    , metresPos
    , metresPos'
    , antipode
    , northPole
    , southPole
    ) where

import Data.Geo.Jord.Ellipsoid (polarRadius)
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres, toMetres)
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model

-- | Geocentric coordinates (cartesian X, Y, Z) of a position in a specified 'Model'.
--
-- @gx-gy@ plane is the equatorial plane, @gx@ is on the prime meridian, and @gz@ on the polar axis.
--
-- On a spherical celestial body, an /n/-vector is equivalent to a normalised version of a
-- geocentric cartesian coordinate.
data Position a =
    Position
        { gx :: Length -- ^ x-coordinate
        , gy :: Length -- ^ y-coordinate
        , gz :: Length -- ^ z-coordinate
        , model :: a -- ^ model (e.g. WGS84)
        }
    deriving (Eq, Show)

-- | 3d vector representing the (X, Y, Z) coordinates in __metres__ of the given position.
metresCoords :: (Model a) => Position a -> Math3d.V3
metresCoords p = coords p Length.toMetres

-- | @coords p f@ returns the 3d vector representing the (X, Y, Z) coordinates in the unit
-- of @f@. For example:
--
-- >>> Geocentric.coords (Geocentric.metresPos 3194669.145061 3194669.145061 4487701.962256 WGS84) Length.toKilometres
-- V3 {vx = 3194.669145061, vy = 3194.669145061, vz = 4487.701962256}
coords :: (Model a) => Position a -> (Length -> Double) -> Math3d.V3
coords (Position x y z _) f = Math3d.vec3 (f x) (f y) (f z)

-- | Geocentric position from given (X, Y, Z) in __metres__ an given 'Model'.
metresPos :: (Model a) => Double -> Double -> Double -> a -> Position a
metresPos xm ym zm = Position (Length.metres xm) (Length.metres ym) (Length.metres zm)

-- | Geocentric position from given 3d vector (X, Y, Z) in __metres__ an given 'Model'.
metresPos' :: (Model a) => Math3d.V3 -> a -> Position a
metresPos' v = metresPos (Math3d.v3x v) (Math3d.v3y v) (Math3d.v3z v)

-- | @antipode p@ computes the antipodal position of @p@: the position which is diametrically
-- opposite to @p@.
antipode :: (Model a) => Position a -> Position a
antipode p = metresPos (Math3d.v3x avm) (Math3d.v3y avm) (Math3d.v3z avm) (model p)
  where
    c = metresCoords p
    avm = Math3d.scale c (-1.0)

-- | Surface position of the North Pole in the given model.
northPole :: (Model a) => a -> Position a
northPole m = metresPos 0 0 r m
  where
    r = Length.toMetres . polarRadius . surface $ m

-- | Surface position of the South Pole in the given model.
southPole :: (Model a) => a -> Position a
southPole m = metresPos 0 0 (-r) m
  where
    r = Length.toMetres . polarRadius . surface $ m
