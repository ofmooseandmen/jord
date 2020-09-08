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
    -- * positions types
      HorizontalPosition
    , Position
    , HasCoordinates(..)
    , height
    , model
    , model'
    -- * Smart constructors
    , latLongPos
    , latLongPos'
    , latLongHeightPos
    , latLongHeightPos'
    , wgs84Pos
    , wgs84Pos'
    , s84Pos
    , s84Pos'
    , nvectorPos
    , nvectorPos'
    , nvectorHeightPos
    , nvectorHeightPos'
    , atHeight
    , atSurface
    -- * Read/Show positions
    , readHorizontalPosition
    , horizontalPosition
    , readPosition
    , position
    -- * /n/-vector conversions
    , nvectorFromLatLong
    , nvectorToLatLong
    -- * Misc.
    , antipode
    , antipode'
    , northPole
    , southPole
    ) where

import Prelude hiding (read)
import Text.ParserCombinators.ReadP (ReadP, option, readP_to_S, skipSpaces)

import Data.Geo.Jord.Angle (Angle)
import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.LatLong as LL (isValidLatLong, isValidLong, latLongDms, showLatLong)
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (length, zero)
import qualified Data.Geo.Jord.Math3d as Math3d (V3, scale, v3x, v3y, v3z, vec3)
import Data.Geo.Jord.Model
import Data.Geo.Jord.Models (S84(..), WGS84(..))

-- | Horizontal coordinates: geodetic latitude, longitude & /n/-vector.
data HCoords =
    HCoords Angle Angle !Math3d.V3

-- | Geodetic coordinates (geodetic latitude, longitude as 'Angle's) of an horizontal position
-- in a specified 'Model'.
--
-- The coordinates are also given as a /n/-vector: the normal vector to the surface.
-- /n/-vector orientation:
--     * z-axis points to the North Pole along the body's rotation axis,
--     * x-axis points towards the point where latitude = longitude = 0
--
-- Note: at the poles all longitudes are equal, therefore a position with a latitude of 90° or -90° will have
-- its longitude forcibly set to 0°.
--
-- The "show" instance gives position in degrees, minutes, seconds, milliseconds ('Angle' "show" instance), and the
-- model ('Model' "show" instance).
--
-- The "eq" instance returns True if and only if, both positions have the same latitude, longitude and model.
-- Note: two positions in different models may represent the same location but are not considered equal.
data HorizontalPosition a =
    HorizontalPosition HCoords a

-- | model of given 'HorizontalPosition' (e.g. WGS84).
model :: (Model a) => HorizontalPosition a -> a
model (HorizontalPosition _ m) = m

-- | Geodetic coordinates (geodetic latitude, longitude as 'Angle's and height as 'Length') of a position
-- in a specified model.
--
-- The "show" instance gives position in degrees, minutes, seconds, milliseconds (HorizontalPosition "show" instance),
-- height ('Length' "show" instance) and the model ('Model' "show" instance).
--
-- The "eq" instance returns True if and only if, both positions have the same horizontal coordinates and height.
--
-- see 'HorizontalPosition'.
data Position a =
    Position HCoords Length a

-- | height of given 'Position' above the surface of the celestial body.
height :: (Model a) => Position a -> Length
height (Position _ h _) = h

-- | model of given 'Position' (e.g. WGS84).
model' :: (Model a) => Position a -> a
model' (Position _ _ m) = m

-- | class for data that provide coordinates.
class HasCoordinates a where
    latitude :: a -> Angle -- ^ geodetic latitude
    decimalLatitude :: a -> Double -- ^ geodetic latitude in decimal degrees
    decimalLatitude = Angle.toDecimalDegrees . latitude
    longitude :: a -> Angle -- ^ longitude
    decimalLongitude :: a -> Double -- ^ longitude  in decimal degrees
    decimalLongitude = Angle.toDecimalDegrees . longitude
    nvector :: a -> Math3d.V3 -- ^ /n/-vector; normal vector to the surface of a celestial body.

instance HasCoordinates (HorizontalPosition a) where
    latitude (HorizontalPosition (HCoords lat _ _) _) = lat
    longitude (HorizontalPosition (HCoords _ lon _) _) = lon
    nvector (HorizontalPosition (HCoords _ _ nv) _) = nv

instance (Model a) => Show (HorizontalPosition a) where
    show p = LL.showLatLong (latitude p, longitude p) ++ " (" ++ (show . model $ p) ++ ")"

-- model equality is ensured by @a@
instance (Model a) => Eq (HorizontalPosition a) where
    p1 == p2 = latitude p1 == latitude p2 && longitude p1 == longitude p2

instance HasCoordinates (Position a) where
    latitude (Position (HCoords lat _ _) _ _) = lat
    longitude (Position (HCoords _ lon _) _ _) = lon
    nvector (Position (HCoords _ _ nv) _ _) = nv

instance (Model a) => Show (Position a) where
    show p =
        LL.showLatLong (latitude p, longitude p) ++
        " " ++ (show . height $ p) ++ " (" ++ (show . model' $ p) ++ ")"

-- model equality is ensured by @a@
instance (Model a) => Eq (Position a) where
    p1 == p2 = latitude p1 == latitude p2 && longitude p1 == longitude p2 && height p1 == height p2

-- | 'HorizontalPosition' from given geodetic latitude & longitude in __decimal degrees__ in the given model.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution with the rest of the
-- API, then wrapped to their respective range.
latLongPos :: (Model a) => Double -> Double -> a -> HorizontalPosition a
latLongPos lat lon = latLongPos' (Angle.decimalDegrees lat) (Angle.decimalDegrees lon)

-- | 'HorizontalPosition' from given geodetic latitude & longitude in the given model.
--
-- Latitude & longitude values are wrapped to their respective range.
latLongPos' :: (Model a) => Angle -> Angle -> a -> HorizontalPosition a
latLongPos' lat lon m = HorizontalPosition (HCoords wlat wlon nv) m
  where
    lon' = checkPole lat lon
    nv = nvectorFromLatLong (lat, lon')
    (wlat, wlon) = wrap lat lon' nv m

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__ and height in the given model
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution with the rest of the
-- API, then wrapped to their respective range.
latLongHeightPos :: (Model a) => Double -> Double -> Length -> a -> Position a
latLongHeightPos lat lon = latLongHeightPos' (Angle.decimalDegrees lat) (Angle.decimalDegrees lon)

-- | 'Position' from given geodetic latitude & longitude and height in the given model.
-- Latitude & longitude values are wrapped to their respective range.
latLongHeightPos' :: (Model a) => Angle -> Angle -> Length -> a -> Position a
latLongHeightPos' lat lon h m = atHeight (latLongPos' lat lon m) h

-- | 'HorizontalPosition' from given geodetic latitude & longitude in __decimal degrees__ in the WGS84 datum.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution with the rest of the
-- API, then wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongPos lat lon WGS84
wgs84Pos :: Double -> Double -> HorizontalPosition WGS84
wgs84Pos lat lon = latLongPos lat lon WGS84

-- | 'HorizontalPosition' from given geodetic latitude & longitude and height in the WGS84 datum.
--
-- Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongPos' lat lon WGS84
wgs84Pos' :: Angle -> Angle -> HorizontalPosition WGS84
wgs84Pos' lat lon = latLongPos' lat lon WGS84

-- | 'HorizontalPosition' from given latitude & longitude in __decimal degrees__ in the spherical datum derived from WGS84.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution with the rest of the
-- API, then wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongPos lat lon S84
s84Pos :: Double -> Double -> HorizontalPosition S84
s84Pos lat lon = latLongPos lat lon S84

-- | 'Position' from given latitude & longitude in the spherical datum derived from WGS84.
--
-- Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- > Geodetic.latLongPos' lat lon h S84
s84Pos' :: Angle -> Angle -> HorizontalPosition S84
s84Pos' lat lon = latLongPos' lat lon S84

-- | 'Position' from given /n/-vector (x, y, z coordinates) in the given model.
--
-- (x, y, z) will be converted first to latitude & longitude to ensure a consistent resolution with the rest of the API.
--
-- This is equivalent to:
--
-- > Geodetic.nvectorPos' (Math3d.vec3 x y z)
nvectorPos :: (Model a) => Double -> Double -> Double -> a -> HorizontalPosition a
nvectorPos x y z = nvectorPos' (Math3d.vec3 x y z)

-- | 'HorizontalPosition' from given /n/-vector (x, y, z coordinates) in the given model.
--
-- (x, y, z) will be converted first to latitude & longitude to ensure a consistent resolution with the rest of the API.
nvectorPos' :: (Model a) => Math3d.V3 -> a -> HorizontalPosition a
nvectorPos' v m = HorizontalPosition (HCoords lat wlon nv) m
  where
    (lat, lon) = nvectorToLatLong v
    lon' = checkPole lat lon
    nv = nvectorFromLatLong (lat, lon')
    wlon = convertLon lon' m

-- | 'Position' from given /n/-vector (x, y, z coordinates) and height in the given model.
--
-- (x, y, z) will be converted first to latitude & longitude to ensure a consistent resolution with the rest of the API.
-- This is equivalent to:
--
-- > Geodetic.nvectorHeightPos' (Math3d.vec3 x y z) h
nvectorHeightPos :: (Model a) => Double -> Double -> Double -> Length -> a -> Position a
nvectorHeightPos x y z = nvectorHeightPos' (Math3d.vec3 x y z)

-- | 'Position' from given /n/-vector (x, y, z coordinates) and height in the given model.
--
-- (x, y, z) will be converted first to latitude & longitude to ensure a consistent resolution with the rest of the API.
nvectorHeightPos' :: (Model a) => Math3d.V3 -> Length -> a -> Position a
nvectorHeightPos' v h m = atHeight (nvectorPos' v m) h

-- | 'Position' from 'HorizontalPosition' & height.
atHeight :: (Model a) => HorizontalPosition a -> Length -> Position a
atHeight (HorizontalPosition c m) h = Position c h m

-- | 'HorizontalPosition' from 'Position'.
atSurface :: (Model a) => Position a -> HorizontalPosition a
atSurface (Position c _ m) = HorizontalPosition c m

-- | Reads an 'HorizontalPosition, from the given string using 'horizontalPosition', for example:
--
-- >>> Geodetic.readHorizontalPosition "55°36'21''N 013°00'02''E" WGS84
-- Just 55°36'21.000"N,13°0'2.000"E (WGS84)
readHorizontalPosition :: (Model a) => String -> a -> Maybe (HorizontalPosition a)
readHorizontalPosition s m =
    case map fst $ filter (null . snd) $ readP_to_S (horizontalPosition m) s of
        [] -> Nothing
        p:_ -> Just p

-- | Reads a 'Position' from the given string using 'position', for example:
--
-- >>> Geodetic.readPosition "55°36'21''N 013°00'02''E 1500m" WGS84
-- Just 55°36'21.000"N,13°0'2.000"E 1500.0m (WGS84)
readPosition :: (Model a) => String -> a -> Maybe (Position a)
readPosition s m =
    case map fst $ filter (null . snd) $ readP_to_S (position m) s of
        [] -> Nothing
        p:_ -> Just p

-- | Parses and returns a 'HorizontalPosition'.
--
-- Supported formats:
--
--     * DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - e.g. 553621N0130002E or 0116S03649E or 47N122W
--
--     * 'Angle'[N|S] 'Angle'[E|W] - e.g. 55°36'21''N 13°0'02''E or 11°16'S 36°49'E or 47°N 122°W
horizontalPosition :: (Model a) => a -> ReadP (HorizontalPosition a)
horizontalPosition m = do
    (lat, lon) <- LL.latLongDms m
    return (latLongPos' lat lon m)

-- | Parses and returns a 'Position': the beginning of the string is parsed by 'horizontalPosition' and additionally the
-- string may end by a valid 'Length'.
position :: (Model a) => a -> ReadP (Position a)
position m = do
    hp <- horizontalPosition m
    skipSpaces
    h <- option Length.zero Length.length
    return (atHeight hp h)

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

-- | @antipode p@ computes the antipodal position of @p@: the position which is diametrically opposite to @p@.
antipode :: (Model a) => HorizontalPosition a -> HorizontalPosition a
antipode p = nvectorPos' (anti . nvector $ p) (model p)

-- | @antipode p@ computes the antipodal position of @p@: the position which is diametrically opposite to @p@ at the
-- same height.
antipode' :: (Model a) => Position a -> Position a
antipode' p = nvectorHeightPos' (anti . nvector $ p) (height p) (model' p)

anti :: Math3d.V3 -> Math3d.V3
anti v = Math3d.scale v (-1.0)

-- | Horizontal position of the North Pole in the given model.
northPole :: (Model a) => a -> HorizontalPosition a
northPole = latLongPos 90 0

-- | Horizontal position of the South Pole in the given model.
southPole :: (Model a) => a -> HorizontalPosition a
southPole = latLongPos (-90) 0

wrap :: (Model a) => Angle -> Angle -> Math3d.V3 -> a -> (Angle, Angle)
wrap lat lon nv m =
    if LL.isValidLatLong lat lon m
        then (lat, lon)
        else llWrapped nv m

llWrapped :: (Model a) => Math3d.V3 -> a -> (Angle, Angle)
llWrapped nv m = (lat, lon')
  where
    (lat, lon) = nvectorToLatLong nv
    lon' = convertLon lon m

convertLon :: (Model a) => Angle -> a -> Angle
convertLon lon m =
    case (longitudeRange m) of
        L180 -> lon
        L360 ->
            if LL.isValidLong lon m
                then lon
                else Angle.add lon (Angle.decimalDegrees 360)

checkPole :: Angle -> Angle -> Angle
checkPole lat lon
    | lat == Angle.decimalDegrees 90 || lat == Angle.decimalDegrees (-90) = Angle.zero
    | otherwise = lon
