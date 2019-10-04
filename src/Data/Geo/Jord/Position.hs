-- |
-- Module:      Data.Geo.Jord.Position
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Position of points in specified models (e.g. WGS84) and conversion functions between
-- coordinate system (geodetic to/from geocentric).
--
-- All functions are implemented using the vector-based approached described in
-- <http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Point_Representation.pdf Gade, K. (2010). A Non-singular Horizontal Position Representation>
--
-- See <http://clynchg3c.com/Technote/geodesy/coorddef.pdf Earth Coordinates>
--
module Data.Geo.Jord.Position
    (
    -- * The 'Position' type
      Position
    , latitude
    , longitude
    , height
    , nvec
    , gcvec
    , model
    -- * /n/-vector
    , NVector
    , nx
    , ny
    , nz
    , nvector
    -- * Geocentric coordinates
    , Geocentric
    , gx
    , gy
    , gz
    , geocentric
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
    , geocentricPos
    , geocentricMetresPos
    , nvh
    -- * Read/Show points
    , readPosition
    , positionP
    -- * Vector3d conversions
    , nvectorFromLatLong
    , nvectorToLatLong
    , nvectorFromGeocentric
    , nvectorToGeocentric
    -- * Misc.
    , antipode
    , latLong
    , latLong'
    , northPole
    , southPole
    , nvNorthPole
    , nvSouthPole
    -- * re-exported for convenience
    , module Data.Geo.Jord.Angle
    , module Data.Geo.Jord.Ellipsoid
    , module Data.Geo.Jord.Ellipsoids
    , module Data.Geo.Jord.LatLong
    , module Data.Geo.Jord.Length
    , module Data.Geo.Jord.Model
    , module Data.Geo.Jord.Models
    , module Data.Geo.Jord.Quantity
    , module Data.Geo.Jord.Vector3d
    ) where

import Text.ParserCombinators.ReadP (ReadP, option, readP_to_S, skipSpaces)

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Ellipsoids
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.Model
import Data.Geo.Jord.Models
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Vector3d

-- | Coordinates of a position in a specified 'Model'.
-- A position provides both geodetic latitude & longitude, height and
-- geocentric coordinates. The horizontal position
-- (i.e. coordinates at the surface of the celestial body) is also provided
-- as /n/-vector.
--
-- The "show" instance gives position in degrees, minutes, seconds,
-- milliseconds ('Angle' "show" instance), height ('Length' "show" instance)
-- and the model ('Model' "show" instance).
--
-- The "eq" instance returns True if and only if, both positions have the same
-- horizontal position, height and model.
data Position a =
    Position
        { latitude :: Angle -- ^ geodetic latitude
        , longitude :: Angle -- ^ geodetic longitude
        , height :: Length -- ^ height above the surface of the celestial body
        , nvec :: !Vector3d -- ^ /n/-vector representing the horizontal coordinates of the position
        , gcvec :: Vector3d -- ^ vector representing the geocentric coordinates of the position (metres)
        , model :: !a -- ^ model (e.g. WGS84)
        }

instance (Model a) => Show (Position a) where
    show p = showLatLong (latitude p, longitude p) ++ " " ++ (show . height $ p) ++ " (" ++ (show . model $ p) ++ ")"

instance (Model a) => Eq (Position a)
    -- model equality is ensure by @a@
                                       where
    p1 == p2 = latitude p1 == latitude p2 && longitude p1 == longitude p2 && height p1 == height p2

-- | normal vector to the surface of a celestial body.
--
-- Orientation: z-axis points to the North Pole along the body's rotation axis,
-- x-axis points towards the point where latitude = longitude = 0.
data NVector =
    NVector Double Double Double

instance Show NVector where
    show (NVector x y z) = "n-vector {" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "}"

-- | x-component of the given /n/-vector.
nx :: NVector -> Double
nx (NVector x _ _) = x

-- | y-component of the given /n/-vector.
ny :: NVector -> Double
ny (NVector _ y _) = y

-- | z-component of the given /n/-vector.
nz :: NVector -> Double
nz (NVector _ _ z) = z

-- | Geocentric (cartesian) coordinates in the fixed-body coordinates system.
--
-- @x-y@ plane is the equatorial plane, @x@ is on the prime meridian, and @z@ on the polar axis.
--
-- On a spherical celestial body, an /n/-vector is equivalent to a normalised version of an
-- geocentric cartesian coordinate.
--
-- Note: For Earth, this is known as the Earth-Centred Earth Fixed coordinates system (ECEF).
--
data Geocentric =
    Geocentric Length Length Length

instance Show Geocentric where
    show (Geocentric x y z) = "geocentric {" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "}"

-- | x-coordinate of the given 'Geocentric' coordinates.
gx :: Geocentric -> Length
gx (Geocentric x _ _) = x

-- | y-coordinate of the given 'Geocentric' coordinates.
gy :: Geocentric -> Length
gy (Geocentric _ y _) = y

-- | z-coordinate of the given 'Geocentric' coordinates.
gz :: Geocentric -> Length
gz (Geocentric _ _ z) = z

-- | @antipode p@ computes the antipodal position of @p@: the position which is diametrically
-- opposite to @p@.
antipode :: (Model a) => Position a -> Position a
antipode p = nvh nv h (model p)
  where
    h = height p
    nv = vscale (nvec p) (-1.0)

-- | @nvector p@ returns the horizontal position of @p@ as a /n/-vector.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> nvector (northPole S84)
-- n-vector {0.0, 0.0, 1.0}
--
-- >>> nvector (wgs84Pos 54 154 (metres 1000))
-- n-vector {-0.5282978852629286, 0.2576680951131586, 0.8090169943749475}
--
nvector :: (Model a) => Position a -> NVector
nvector p = NVector x y z
  where
    (Vector3d x y z) = nvec p

-- | @geocentric p@ returns the 'Geocentric' coordinates of position @p@.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> geocentric (wgs84Pos 54 154 (metres 1000))
-- geocentric {-3377.4908375km, 1647.312349km, 5137.5528484km}
--
geocentric :: (Model a) => Position a -> Geocentric
geocentric p = Geocentric (metres x) (metres y) (metres z)
  where
    (Vector3d x y z) = gcvec p

-- | Horizontal position of the North Pole in the given model.
northPole :: (Model a) => a -> Position a
northPole = nvh nvNorthPole zero

-- | Horizontal position of the South Pole in the given model.
southPole :: (Model a) => a -> Position a
southPole = nvh nvSouthPole zero

-- | Horizontal position of the North Pole (/n/-vector).
nvNorthPole :: Vector3d
nvNorthPole = Vector3d 0.0 0.0 1.0

-- | Horizontal position of the South Pole (/n/-vector).
nvSouthPole :: Vector3d
nvSouthPole = Vector3d 0.0 0.0 (-1.0)

-- | Reads a 'Position' from the given string using 'positionP'.
readPosition :: (Model a) => String -> a -> Maybe (Position a)
readPosition s m =
    case map fst $ filter (null . snd) $ readP_to_S (positionP m) s of
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
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> readPosition "55°36'21''N 013°00'02''E" WGS84
-- Just 55°36'21.000"N,13°0'2.000"E 0.0m (WGS84)
-- >>>
-- >>> readPosition "55°36'21''N 013°00'02''E 1500m" WGS84
-- Just 55°36'21.000"N,13°0'2.000"E 1500.0m (WGS84)
--
positionP :: (Model a) => a -> ReadP (Position a)
positionP m = do
    (lat, lon) <- latLongDmsP m
    skipSpaces
    h <- option zero lengthP
    return (latLongHeightPos' lat lon h m)

-- | Ground 'Position' from given geodetic latitude & longitude in __decimal degrees__ in
-- the given model.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution
-- with the rest of the API, then wrapped to their respective range.
--
-- This is equivalent to:
--
-- @
--     'latLongHeightPos' lat lon zero model
-- @
--
latLongPos :: (Model a) => Double -> Double -> a -> Position a
latLongPos lat lon = latLongHeightPos lat lon zero

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__ and height
-- in the given model
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution
-- with the rest of the API, then wrapped to their respective range.
--
latLongHeightPos :: (Model a) => Double -> Double -> Length -> a -> Position a
latLongHeightPos lat lon = latLongHeightPos' (decimalDegrees lat) (decimalDegrees lon)

-- | Ground 'Position' from given geodetic latitude & longitude in
-- the given model.
-- Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- @
--     'latLongHeightPos'' lat lon zero model
-- @
--
latLongPos' :: (Model a) => Angle -> Angle -> a -> Position a
latLongPos' lat lon = latLongHeightPos' lat lon zero

-- | 'Position' from given geodetic latitude & longitude and height in the given model.
-- Latitude & longitude values are wrapped to their respective range.
latLongHeightPos' :: (Model a) => Angle -> Angle -> Length -> a -> Position a
latLongHeightPos' lat lon h m = Position lat' lon' h nv g m
  where
    nv = nvectorFromLatLong (lat, lon)
    g = nvectorToGeocentric (nv, h) (surface m)
    (lat', lon') = wrap lat lon nv m

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__ and height in
-- the WGS84 datum.
--
-- Latitude & longitude values are first converted to 'Angle' to ensure a consistent resolution
-- with the rest of the API, then wrapped to their respective range.
--
-- This is equivalent to:
--
-- @
--     'latLongHeightPos' lat lon h 'WGS84'
-- @
--
wgs84Pos :: Double -> Double -> Length -> Position WGS84
wgs84Pos lat lon h = latLongHeightPos lat lon h WGS84

-- | 'Position' from given geodetic latitude & longitude and height in the WGS84 datum.
-- Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- @
--     'latLongHeightPos'' lat lon h 'WGS84'
-- @
--
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
-- @
--     'latLongHeightPos' lat lon h 'S84'
-- @
--
s84Pos :: Double -> Double -> Length -> Position S84
s84Pos lat lon h = latLongHeightPos lat lon h S84

-- | 'Position' from given latitude & longitude and height in the spherical datum derived
-- from WGS84. Latitude & longitude values are wrapped to their respective range.
--
-- This is equivalent to:
--
-- @
--     'latLongHeightPos'' lat lon h 'S84'
-- @
--
s84Pos' :: Angle -> Angle -> Length -> Position S84
s84Pos' lat lon h = latLongHeightPos' lat lon h S84

-- | 'Position' from given geocentric coordinates x, y and z in the given model.
geocentricPos :: (Model a) => Length -> Length -> Length -> a -> Position a
geocentricPos x y z = geocentricMetresPos' (toMetres x) (toMetres y) (toMetres z)

-- | 'Position' from given geocentric coordinates x, y and z in __metres__ in the given model.
--
-- x, y, z lengths are first converted to 'Length' to ensure a consistent resolution with the rest of the API.
geocentricMetresPos :: (Model a) => Double -> Double -> Double -> a -> Position a
geocentricMetresPos x y z = geocentricMetresPos' (toMetres . metres $ x) (toMetres . metres $ y) (toMetres . metres $ z)

geocentricMetresPos' :: (Model a) => Double -> Double -> Double -> a -> Position a
geocentricMetresPos' x y z m = Position lat lon h nv ev m
  where
    ev = Vector3d x y z
    (nv, h) = nvectorFromGeocentric ev (surface m)
    (lat, lon) = nvectorToLatLong nv

-- | 'Position' from given /n/-vector x, y, z coordinates in the given model.
-- Vector (x, y, z) will be normalised to a unit vector to get a valid /n/-vector.
--
-- This is equivalent to:
--
-- @
--     'nvectorHeightPos' lat lon zero model
-- @
--
nvectorPos :: (Model a) => Double -> Double -> Double -> a -> Position a
nvectorPos x y z = nvectorHeightPos x y z zero

-- | 'Position' from given /n/-vector x, y, z coordinates and height in the given model.
-- Vector (x, y, z) will be normalised to a unit vector to get a valid /n/-vector.
nvectorHeightPos :: (Model a) => Double -> Double -> Double -> Length -> a -> Position a
nvectorHeightPos x y z = nvh (vunit (Vector3d x y z))

-- | (latitude, longitude) pair in __decimal degrees__ from given position.
latLong :: (Model a) => Position a -> (Double, Double)
latLong p = (toDecimalDegrees . latitude $ p, toDecimalDegrees . longitude $ p)

-- | (latitude, longitude) pair from given position.
latLong' :: (Model a) => Position a -> (Angle, Angle)
latLong' p = (latitude p, longitude p)
 -- given 'Vector3d' is a /n/-vector.

-- | position from /n/-vector, height and model; this method is to be used only if
nvh :: (Model a) => Vector3d -> Length -> a -> Position a
nvh nv h m = Position lat lon h nv g m
  where
    (lat, lon) = llWrapped nv (longitudeRange m)
    g = nvectorToGeocentric (nv, h) (surface m)

-- | @nvectorToLatLong nv@ returns (latitude, longitude) pair equivalent to the given /n/-vector @nv@.
--
-- You should prefer using:
--
-- @
--     'latLong' ('nvectorPos' x y z model)
-- @
--
-- Latitude is always in [-90°, 90°] and longitude in [-180°, 180°].
nvectorToLatLong :: Vector3d -> (Angle, Angle)
nvectorToLatLong nv = (lat, lon)
  where
    lat = atan2' (vz nv) (sqrt (vx nv * vx nv + vy nv * vy nv))
    lon = atan2' (vy nv) (vx nv)

-- | @nvectorFromLatLong ll@ returns /n/-vector equivalent to the given (latitude, longitude) pair @ll@.
--
-- You should prefer using:
--
-- @
--     'nvector' ('latLongPos' lat lon model)
-- @
nvectorFromLatLong :: (Angle, Angle) -> Vector3d
nvectorFromLatLong (lat, lon) = Vector3d x y z
  where
    cl = cos' lat
    x = cl * cos' lon
    y = cl * sin' lon
    z = sin' lat

-- | @nvectorToGeocentric (nv, h) e@ returns the geocentric coordinates equivalent to the given
-- /n/-vector @nv@ and height @h@ using the ellispoid @e@.
--
-- You should prefer using:
--
-- @
--     'geocentric' ('nvectorHeightPos' x y z h model)
-- @
--
nvectorToGeocentric :: (Vector3d, Length) -> Ellipsoid -> Vector3d
nvectorToGeocentric (nv, h) e
    | isSphere e = nvectorToGeocentricS (nv, h) (equatorialRadius e)
    | otherwise = nvectorToGeocentricE (nv, h) e

nvectorToGeocentricS :: (Vector3d, Length) -> Length -> Vector3d
nvectorToGeocentricS (nv, h) r = vscale nv (toMetres n)
  where
    n = add h r

nvectorToGeocentricE :: (Vector3d, Length) -> Ellipsoid -> Vector3d
nvectorToGeocentricE (nv, h) e = Vector3d gx' gy' gz'
  where
    a = toMetres . equatorialRadius $ e
    b = toMetres . polarRadius $ e
    nx' = vx nv
    ny' = vy nv
    nz' = vz nv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx' * nx' * m) + (ny' * ny' * m) + (nz' * nz'))
    h' = toMetres h
    gx' = n * m * nx' + h' * nx'
    gy' = n * m * ny' + h' * ny'
    gz' = n * nz' + h' * nz'

-- | @nvectorFromGeocentric g e@ returns the /n/-vector equivalent to the geocentric
-- coordinates @g@ using the ellispoid @e@.
--
-- You should prefer using:
--
-- @
--     'nvector' ('geocentricMetresPos' x y z model)
-- @
--
nvectorFromGeocentric :: Vector3d -> Ellipsoid -> (Vector3d, Length)
nvectorFromGeocentric g e
    | isSphere e = nvectorFromGeocentricS g (equatorialRadius e)
    | otherwise = nvectorFromGeocentricE g e

nvectorFromGeocentricS :: Vector3d -> Length -> (Vector3d, Length)
nvectorFromGeocentricS g r = (vunit g, h)
  where
    h = sub (metres (vnorm g)) r

nvectorFromGeocentricE :: Vector3d -> Ellipsoid -> (Vector3d, Length)
nvectorFromGeocentricE g e = (nvecEllipsoidal d e2 k px py pz, metres h)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = toMetres . equatorialRadius $ e
    a2 = a * a
    px = vx g
    py = vy g
    pz = vz g
    p = (px * px + py * py) / a2
    q = ((1 - e2) / a2) * (pz * pz)
    r = (p + q - e4) / 6.0
    s = (e4 * p * q) / (4.0 * r * r * r)
    t = (1.0 + s + sqrt (s * (2.0 + s))) ** (1 / 3)
    u = r * (1.0 + t + 1.0 / t)
    v = sqrt (u * u + q * e4)
    w = e2 * (u + v - q) / (2.0 * v)
    k = sqrt (u + v + w * w) - w
    d = k * sqrt (px * px + py * py) / (k + e2)
    h = ((k + e2 - 1.0) / k) * sqrt (d * d + pz * pz)

nvecEllipsoidal :: Double -> Double -> Double -> Double -> Double -> Double -> Vector3d
nvecEllipsoidal d e2 k px py pz = Vector3d nx' ny' nz'
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx' = s * a * px
    ny' = s * a * py
    nz' = s * pz

wrap :: (Model a) => Angle -> Angle -> Vector3d -> a -> (Angle, Angle)
wrap lat lon nv m =
    if isValidLatLong lat lon m
        then (lat, lon)
        else llWrapped nv (longitudeRange m)

llWrapped :: Vector3d -> LongitudeRange -> (Angle, Angle)
llWrapped nv lr = (lat, lon')
  where
    (lat, lon) = nvectorToLatLong nv
    lon' =
        case lr of
            L180 -> lon
            L360 -> add lon (decimalDegrees 180)
