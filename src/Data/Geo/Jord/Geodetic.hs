-- |
-- Module:      Data.Geo.Jord.Geodetic
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geodetic coordinates of points in specified models (e.g. WGS84) and conversion functions between
-- /n/-vectors and latitude/longitude.
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Point_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
-- In order to use this module you should start with the following imports:
--
-- @
-- import qualified Data.Geo.Jord.Geodetic as Geodetic
-- import qualified Data.Geo.Jord.Length as Length
-- import Data.Geo.Jord.Models
-- @
module Data.Geo.Jord.Geodetic
    (
    -- * The 'Position' type
      Position
    , latitude
    , longitude
    , height
    , nvector
    , model
    , llEq
    -- * Smart constructors
    , latLongPos
    , latLongHeightPos
    , latLongPos'
    , latLongHeightPos'
    , wgs84Pos
    , wgs84Pos'
    , s84Pos
    , s84Pos'
    , nvectorPos
    , nvectorHeightPos
    , nvectorPos'
    , nvectorHeightPos'
    -- * Read/Show positions
    , read
    , position
    -- * /n/-vector conversions
    , nvectorFromLatLong
    , nvectorToLatLong
    -- * Misc.
    , antipode
    , northPole
    , southPole
    ) where

import Prelude hiding (read)
import Text.ParserCombinators.ReadP (ReadP, option, readP_to_S, skipSpaces)

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle (add, atan2, cos, decimalDegrees, sin)
import qualified Data.Geo.Jord.LatLong as LL (isValidLatLong, latLongDms, showLatLong)
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (length, zero)
import qualified Data.Geo.Jord.Math3d as Math3d (V3, scale, v3x, v3y, v3z, vec3)
import Data.Geo.Jord.Model
import Data.Geo.Jord.Models (S84(..), WGS84(..))

-- | Geodetic coordinates (geodetic latitude, longitude and height) of a position
-- in a specified 'Model'.
--
-- The coordinates at the surface of the celestial body are also given as a /n/-vector:
-- the normal vector to the surface.
-- /n/-vector orientation:
--     * z-axis points to the North Pole along the body's rotation axis,
--     * x-axis points towards the point where latitude = longitude = 0
--
-- The "show" instance gives position in degrees, minutes, seconds,
-- milliseconds ('Angle' "show" instance), height ('Length' "show" instance)
-- and the model ('Model' "show" instance).
--
-- The "eq" instance returns True if and only if, both positions have the same
-- latitude, longitude, height and model. Note: two positions in different models
-- may represent the same location but are not considered equal.
data Position a =
    Position
        { latitude :: Angle -- ^ geodetic latitude
        , longitude :: Angle -- ^ longitude
        , height :: Length -- ^ height above the surface of the celestial body
        , nvector :: !Math3d.V3 -- ^ /n/-vector; normal vector to the surface of a celestial body.
        , model :: !a -- ^ model (e.g. WGS84)
        }

instance (Model a) => Show (Position a) where
    show p =
        LL.showLatLong (latitude p, longitude p) ++
        " " ++ (show . height $ p) ++ " (" ++ (show . model $ p) ++ ")"

-- model equality is ensured by @a@
instance (Model a) => Eq (Position a) where
    p1 == p2 = latitude p1 == latitude p2 && longitude p1 == longitude p2 && height p1 == height p2

-- | both position have same latitude and longitude irrespective of height ?
llEq :: (Model a) => Position a -> Position a -> Bool
llEq p1 p2 = latitude p1 == latitude p2 && longitude p1 == longitude p2

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__ at
-- the surface of the given model.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution
-- with the rest of the API, then wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongHeightPos lat lon Length.zero model
latLongPos :: (Model a) => Double -> Double -> a -> Position a
latLongPos lat lon = latLongHeightPos lat lon Length.zero

-- | 'Position' from given geodetic latitude & longitude in at the surface of
-- the given model.
--
-- Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongHeightPos' lat lon Length.zero model
latLongPos' :: (Model a) => Angle -> Angle -> a -> Position a
latLongPos' lat lon = latLongHeightPos' lat lon Length.zero

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__ and height
-- in the given model
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution
-- with the rest of the API, then wrapped to their respective range.
latLongHeightPos :: (Model a) => Double -> Double -> Length -> a -> Position a
latLongHeightPos lat lon = latLongHeightPos' (Angle.decimalDegrees lat) (Angle.decimalDegrees lon)

-- | 'Position' from given geodetic latitude & longitude and height in the given model.
-- Latitude & longitude values are wrapped to their respective range.
latLongHeightPos' :: (Model a) => Angle -> Angle -> Length -> a -> Position a
latLongHeightPos' lat lon h m = Position lat' lon' h nv m
  where
    nv = nvectorFromLatLong (lat, lon)
    (lat', lon') = wrap lat lon nv m

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__ and height in
-- the WGS84 datum.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution
-- with the rest of the API, then wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongHeightPos lat lon h WGS84
wgs84Pos :: Double -> Double -> Length -> Position WGS84
wgs84Pos lat lon h = latLongHeightPos lat lon h WGS84

-- | 'Position' from given geodetic latitude & longitude and height in the WGS84 datum.
-- Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongHeightPos' lat lon h WGS84
wgs84Pos' :: Angle -> Angle -> Length -> Position WGS84
wgs84Pos' lat lon h = latLongHeightPos' lat lon h WGS84

-- | 'Position' from given latitude & longitude in __decimal degrees__ and height in the
-- spherical datum derived from WGS84.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution
-- with the rest of the API, then wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongHeightPos lat lon h S84
s84Pos :: Double -> Double -> Length -> Position S84
s84Pos lat lon h = latLongHeightPos lat lon h S84

-- | 'Position' from given latitude & longitude and height in the spherical datum derived
-- from WGS84. Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongHeightPos' lat lon h S84
s84Pos' :: Angle -> Angle -> Length -> Position S84
s84Pos' lat lon h = latLongHeightPos' lat lon h S84

-- | 'Position' from given /n/-vector x, y, z coordinates in the given model.
-- (x, y, z) will be converted to latitude & longitude to ensure a consistent resolution
-- with the rest of the API.
--
-- This is equivalent to:
--
-- > Geodetic.nvectorPos' (Math3d.vec3 x y z)
nvectorPos :: (Model a) => Double -> Double -> Double -> a -> Position a
nvectorPos x y z = nvectorPos' (Math3d.vec3 x y z)

-- | 'Position' from given /n/-vector x, y, z coordinates and height in the given model.
-- (x, y, z) will be converted to latitude & longitude to ensure a consistent resolution
-- with the rest of the API.
-- This is equivalent to:
--
-- > Geodetic.nvectorHeightPos' (Math3d.vec3 x y z) h
nvectorHeightPos :: (Model a) => Double -> Double -> Double -> Length -> a -> Position a
nvectorHeightPos x y z = nvectorHeightPos' (Math3d.vec3 x y z)

-- | 'Position' from given /n/-vector x, y, z coordinates in the given model.
-- (x, y, z) will be converted to latitude & longitude to ensure a consistent resolution
-- with the rest of the API.
--
-- This is equivalent to:
--
-- > Geodetic.nvectorHeightPos' lat lon Length.zero model
nvectorPos' :: (Model a) => Math3d.V3 -> a -> Position a
nvectorPos' v = nvectorHeightPos' v Length.zero

-- | 'Position' from given /n/-vector x, y, z coordinates and height in the given model.
-- (x, y, z) will be converted to latitude & longitude to ensure a consistent resolution
-- with the rest of the API.
nvectorHeightPos' :: (Model a) => Math3d.V3 -> Length -> a -> Position a
nvectorHeightPos' v h = Position lat lon h nv
  where
    ll@(lat, lon) = nvectorToLatLong v
    nv = nvectorFromLatLong ll

-- | Reads a 'Position' from the given string using 'position'.
--
-- ==== __Examples__
--
-- >>> Geodetic.read "55°36'21''N 013°00'02''E" WGS84
-- Just 55°36'21.000"N,13°0'2.000"E 0.0m (WGS84)
-- >>>
-- >>> Geodetic.read "55°36'21''N 013°00'02''E 1500m" WGS84
-- Just 55°36'21.000"N,13°0'2.000"E 1500.0m (WGS84)
read :: (Model a) => String -> a -> Maybe (Position a)
read s m =
    case map fst $ filter (null . snd) $ readP_to_S (position m) s of
        [] -> Nothing
        p:_ -> Just p

-- | Parses and returns a 'Position'.
--
-- Supported formats:
--
--     * DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - e.g. 553621N0130002E or 0116S03649E or 47N122W
--
--     * 'Angle'[N|S] 'Angle'[E|W] - e.g. 55°36'21''N 13°0'02''E or 11°16'S 36°49'E or 47°N 122°W
--
-- Additionally the string may end by a valid 'Length'.
position :: (Model a) => a -> ReadP (Position a)
position m = do
    (lat, lon) <- LL.latLongDms m
    skipSpaces
    h <- option Length.zero Length.length
    return (latLongHeightPos' lat lon h m)

-- | @nvectorToLatLong nv@ returns (latitude, longitude) pair equivalent to the given /n/-vector @nv@.
-- Latitude is always in [-90°, 90°] and longitude in [-180°, 180°].
nvectorToLatLong :: Math3d.V3 -> (Angle, Angle)
nvectorToLatLong v = (lat, lon)
  where
    x = Math3d.v3x v
    y = Math3d.v3y v
    z = Math3d.v3z v
    lat = Angle.atan2 z (sqrt (x * x + y * y))
    lon = Angle.atan2 y x

-- | @nvectorFromLatLong ll@ returns /n/-vector equivalent to the given (latitude, longitude) pair @ll@.
nvectorFromLatLong :: (Angle, Angle) -> Math3d.V3
nvectorFromLatLong (lat, lon) = Math3d.vec3 x y z
  where
    cl = Angle.cos lat
    x = cl * Angle.cos lon
    y = cl * Angle.sin lon
    z = Angle.sin lat

-- | @antipode p@ computes the antipodal position of @p@: the position which is diametrically
-- opposite to @p@.
antipode :: (Model a) => Position a -> Position a
antipode p = nvectorHeightPos' nv h (model p)
  where
    h = height p
    nv = Math3d.scale (nvector p) (-1.0)

-- | Surface position of the North Pole in the given model.
northPole :: (Model a) => a -> Position a
northPole = latLongHeightPos 90 0 Length.zero

-- | Surface position of the South Pole in the given model.
southPole :: (Model a) => a -> Position a
southPole = latLongHeightPos (-90) 0 Length.zero

wrap :: (Model a) => Angle -> Angle -> Math3d.V3 -> a -> (Angle, Angle)
wrap lat lon nv m =
    if LL.isValidLatLong lat lon m
        then (lat, lon)
        else llWrapped nv (longitudeRange m)

llWrapped :: Math3d.V3 -> LongitudeRange -> (Angle, Angle)
llWrapped nv lr = (lat, lon')
  where
    (lat, lon) = nvectorToLatLong nv
    lon' =
        case lr of
            L180 -> lon
            L360 -> Angle.add lon (Angle.decimalDegrees 180)
