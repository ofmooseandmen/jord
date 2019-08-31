-- |
-- Module:      Data.Geo.Jord.Position
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- TODO
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
    , evec
    , model
    -- * /n/-vector
    , NVector
    , nx
    , ny
    , nz
    -- * ECEF
    , EcefVector
    , ex
    , ey
    , ez
    -- * smart constructors
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
    , ecefPos
    , ecefMetresPos
    , nvh
    -- * Conversions/Transformations
    , nvectorToLatLong
    , nvectorFromLatLong
    , nvectorToEcef
    , nvectorFromEcef
    -- * Read/Show points
    , readPosition
    , positionP
    -- * Misc.
    , nvector
    , ecef
    , latLong
    , latLong'
    , northPole
    , southPole
    ) where

import Text.ParserCombinators.ReadP (ReadP, option, readP_to_S, skipSpaces)

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Bodies
import Data.Geo.Jord.Internal
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Vector3d

-- | 3-D position relative to a celestial body represented by a specified
-- model (spherical or ellipsoidal).
-- A position provides both geodetic latitude & longitude, height and
-- Earth Centred Earth Fixed (ECEF) coordinates. The horizontal position
-- (i.e. coordinates at the surface of the celestial body) are also provided
-- as /n/-vector.
--
-- The "show" instance gives position in degrees, minutes, seconds,
-- milliseconds ('Angle' "show" instance), height ('Length' "show" instance)
-- and the model ('Model' "show" instance).
--
-- The "eq" instance returns True if and only if, both positions have the same
-- horizontal position, height and model.
data Position a = Position
    { latitude :: Angle -- ^ geodetic latitude
    , longitude :: Angle -- ^ geodetic longitude
    , height :: Length -- ^ height above the surface of the celestial body
    , nvec :: !Vector3d -- ^ /n/-vector representing the horizontal coordinates of the position
    , evec :: Vector3d -- ^ ECEF vector representing the ECEF coordinates of the position
    , model :: !a -- ^ model (e.g. WGS84)
    }

instance (Model a) => Show (Position a) where
    show p =
        showLatLong (latitude p, longitude p) ++
        " " ++ (show . height $ p) ++ " (" ++ (show . model $ p) ++ ")"

instance (Model a) => Eq (Position a) where
    p1 == p2 =
        latitude p1 == latitude p2 &&
        longitude p1 == longitude p2 && height p1 == height p2 && model p1 == model p2

-- | normal vector to the celestial body model.
--
-- Orientation: z-axis points to the North Pole along the body's rotation axis,
-- x-axis points towards the point where latitude = longitude = 0.
data NVector =
    NVector Double
            Double
            Double

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

-- | Position coordinates in the Earth Centred, Earth Fixed (ECEF) coordinates system.
--
-- @ex-ey@ plane is the equatorial plane, @ex@ is on the prime meridian, and @ez@ on the polar axis.
--
-- Note: on a spherical model earth, an /n/-vector is equivalent to a normalised version of an (ECEF) cartesian coordinate.
data EcefVector =
    EcefVector Length
               Length
               Length

instance Show EcefVector where
    show (EcefVector x y z) = "ecef {" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "}"

-- | x-component of the given 'EcefVector'.
ex :: EcefVector -> Length
ex (EcefVector x _ _) = x

-- | y-component of the given 'EcefVector'.
ey :: EcefVector -> Length
ey (EcefVector _ y _) = y

-- | z-component of the given 'EcefVector'.
ez :: EcefVector -> Length
ez (EcefVector _ _ z) = z

-- | @nvector p@ returns the horizontal position of position @p@ as a /n/-vector.
nvector :: (Model a) => Position a -> NVector
nvector p = NVector x y z
  where
    (Vector3d x y z) = nvec p

-- | @ecef p@ returns the ECEF coordinates of position @p@.
ecef :: (Model a) => Position a -> EcefVector
ecef p = EcefVector (metres x) (metres y) (metres z)
  where
    (Vector3d x y z) = evec p

-- | surface position of the North Pole for the specified model.
northPole :: (Model a) => a -> Position a
northPole = nvh nvNorthPole zero

-- | surface position of the South Pole for the specified model.
southPole :: (Model a) => a -> Position a
southPole = nvh nvSouthPole zero

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
-- @
--     readPosition "55°36'21''N 013°00'02''E" WGS84
--     -- Just 55°36'21.000"N,13°0'2.000"E 0.0m (WGS84)
--     readPosition "55°36'21''N 013°00'02''E 1500m" WGS84
--     -- Just 55°36'21.000"N,13°0'2.000"E 1500.0m (WGS84)
-- @
positionP :: (Model a) => a -> ReadP (Position a)
positionP m = do
    (lat, lon) <- latLongDmsP m
    skipSpaces
    h <- option zero lengthP
    return (latLongHeightPos' lat lon h m)

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__
-- at the surface of the given model.
--
-- latitude & longitude values are wrapped to their respective range.
latLongPos :: (Model a) => Double -> Double -> a -> Position a
latLongPos lat lon = latLongHeightPos lat lon zero

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__
-- and height around the surface of the given model.
--
-- latitude & longitude values are wrapped to their respective range.
latLongHeightPos :: (Model a) => Double -> Double -> Length -> a -> Position a
latLongHeightPos lat lon = latLongHeightPos' (decimalDegrees lat) (decimalDegrees lon)

-- | 'Position' from given geodetic latitude & longitude at the surface of the
-- given model.
--
-- latitude & longitude values are wrapped to their respective range.
latLongPos' :: (Model a) => Angle -> Angle -> a -> Position a
latLongPos' lat lon = latLongHeightPos' lat lon zero

-- | 'Position' from given geodetic latitude & longitude and height around
-- the surface of the given model.
--
-- latitude & longitude values are wrapped to their respective range.
-- FIXME: handle latitude and/or longitude outside range
latLongHeightPos' :: (Model a) => Angle -> Angle -> Length -> a -> Position a
latLongHeightPos' lat lon h m = Position lat lon h nv e m
  where
    nv = nvectorFromLatLong' (lat, lon)
    e = nvectorToEcef' (nv, h) (shape m)

-- | 'Position' from given geodetic latitude & longitude in __decimal degrees__
-- and height around the surface of the WGS84 ellispoid.
--
-- latitude & longitude values are wrapped to their respective range.
wgs84Pos :: Double -> Double -> Length -> Position WGS84
wgs84Pos lat lon h = latLongHeightPos lat lon h WGS84

-- | 'Position' from given geodetic latitude & longitude and height around
-- the surface of the WGS84 ellispoid.
--
-- latitude & longitude values are wrapped to their respective range.
wgs84Pos' :: Angle -> Angle -> Length -> Position WGS84
wgs84Pos' lat lon h = latLongHeightPos' lat lon h WGS84

-- | 'Position' from given latitude & longitude in __decimal degrees__
-- and height around the surface of the sphere derived from WGS84 ellispoid.
--
-- latitude & longitude values are wrapped to their respective range.
s84Pos :: Double -> Double -> Length -> Position S84
s84Pos lat lon h = latLongHeightPos lat lon h S84

-- | 'Position' from given latitude & longitude and height around the surface
-- of the sphere derived from WGS84 ellispoid.
--
-- latitude & longitude values are wrapped to their respective range.
s84Pos' :: Angle -> Angle -> Length -> Position S84
s84Pos' lat lon h = latLongHeightPos' lat lon h S84

ecefPos :: (Model a) => Length -> Length -> Length -> a -> Position a
ecefPos x y z = ecefMetresPos (toMetres x) (toMetres y) (toMetres z)

ecefMetresPos :: (Model a) => Double -> Double -> Double -> a -> Position a
ecefMetresPos x y z m = Position lat lon h nv ev m
  where
    ev = Vector3d x y z
    (nv, h) = nvectorFromEcef' ev (shape m)
    (lat, lon) = nvectorToLatLong' nv

nvectorPos :: (Model a) => Double -> Double -> Double -> a -> Position a
nvectorPos x y z = nvectorHeightPos x y z zero

nvectorHeightPos :: (Model a) => Double -> Double -> Double -> Length -> a -> Position a
nvectorHeightPos x y z = nvh (vunit (Vector3d x y z))

nvectorToLatLong :: NVector -> (Angle, Angle)
nvectorToLatLong nv = nvectorToLatLong' (Vector3d (nx nv) (ny nv) (nz nv))

nvectorFromLatLong :: (Angle, Angle) -> NVector
nvectorFromLatLong ll = NVector x y z
  where
    (Vector3d x y z) = nvectorFromLatLong' ll

nvectorToEcef :: (NVector, Length) -> Shape -> EcefVector
nvectorToEcef (nv, h) s = EcefVector (metres x) (metres y) (metres z)
  where
    v = Vector3d (nx nv) (ny nv) (nz nv)
    (Vector3d x y z) = nvectorToEcef' (v, h) s

nvectorFromEcef :: EcefVector -> Shape -> (NVector, Length)
nvectorFromEcef ev s = (NVector x y z, h)
  where
    v = Vector3d (toMetres . ex $ ev) (toMetres . ey $ ev) (toMetres . ez $ ev)
    (Vector3d x y z, h) = nvectorFromEcef' v s

latLong :: (Model a) => Position a -> (Double, Double)
latLong p = (toDecimalDegrees . latitude $ p, toDecimalDegrees . longitude $ p)

latLong' :: (Model a) => Position a -> (Angle, Angle)
latLong' p = (latitude p, longitude p)

nvh :: (Model a) => Vector3d -> Length -> a -> Position a
nvh nv h m = Position lat lon h nv e m
  where
    (lat, lon) = nvectorToLatLong' nv
    e = nvectorToEcef' (nv, h) (shape m)

nvectorToLatLong' :: Vector3d -> (Angle, Angle)
nvectorToLatLong' nv = (lat, lon)
  where
    lat = atan2' (vz nv) (sqrt (vx nv * vx nv + vy nv * vy nv))
    lon = atan2' (vy nv) (vx nv)

nvectorFromLatLong' :: (Angle, Angle) -> Vector3d
nvectorFromLatLong' (lat, lon) = Vector3d x y z
  where
    cl = cos' lat
    x = cl * cos' lon
    y = cl * sin' lon
    z = sin' lat

nvectorToEcef' :: (Vector3d, Length) -> Shape -> Vector3d
nvectorToEcef' (nv, h) (Sphere r) = ev
  where
    n = add h r
    ev = vscale nv (toMetres n)
nvectorToEcef' (nv, h) (Ellipsoid e) = Vector3d ex' ey' ez'
  where
    a = toMetres . equatorialRadius $ e
    b = toMetres . polarRadius $ e
    nx' = vx nv
    ny' = vy nv
    nz' = vz nv
    m = (a * a) / (b * b)
    n = b / sqrt ((nx' * nx' * m) + (ny' * ny' * m) + (nz' * nz'))
    h' = toMetres h
    ex' = n * m * nx' + h' * nx'
    ey' = n * m * ny' + h' * ny'
    ez' = n * nz' + h' * nz'

nvectorFromEcef' :: Vector3d -> Shape -> (Vector3d, Length)
nvectorFromEcef' ev (Sphere r) = (vunit ev, h)
  where
    h = sub (metres (vnorm ev)) r
nvectorFromEcef' ev (Ellipsoid e) = (nvecEllipsoidal d e2 k px py pz, metres h)
  where
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = toMetres . equatorialRadius $ e
    a2 = a * a
    px = vx ev
    py = vy ev
    pz = vz ev
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
