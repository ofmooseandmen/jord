-- |
-- Module:      Data.Geo.Jord.GeoPos
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types to represent a geographic position by its latitude and longitude.
--
module Data.Geo.Jord.GeoPos
    ( Degrees(..)
    , GeoPos(latitude, longitude)
    , geo
    , readGeo
    , readGeoF
    , readGeoM
    ) where

import Control.Applicative hiding (many)
import Control.Monad.Fail
import Data.Char
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)

-- | Degrees.
newtype Degrees = Degrees
    { degrees :: Double
    } deriving (Eq, Show)

-- | A geographic position (latitude and longitude in decimal degrees).
data GeoPos = GeoPos
    { latitude :: Degrees
    , longitude :: Degrees
    } deriving (Eq)

-- | See 'readGeo'.
instance Read GeoPos where
    readsPrec _ = readP_to_S geo'

-- | Produced string format: d°(m')(s'')[N|S] d°(m')(s'')[E|W] - e.g. 55°36'21''N 13°0'02''E.
instance Show GeoPos where
    show (GeoPos lat lon) = showLat (degrees lat) ++ " " ++ showLon (degrees lon)

-- | 'GeoPos' smart constructor.
-- Calls 'error' if given latitude is outisde [-90, 90]° and/or
-- given longitude is outisde [-180, 180]°.
geo :: Double -> Double -> GeoPos
geo lat lon
    | not (isValidLatitude lat) = error ("invalid latitude=" ++ show lat)
    | not (isValidLongitude lon) = error ("invalid longitude=" ++ show lon)
    | otherwise = GeoPos (Degrees lat) (Degrees lon)

-- | Obtains a 'GeoPos' from the given string formatted as either:
--
--     * DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - e.g. 553621N0130002E or 0116S03649E or 47N122W
--
--     * d°(m')(s'')[N|S] d°(m')(s'')[E|W] - e.g. 55°36'21''N 13°0'02''E or 11°16'S 36°49'E or 47°N 122°W
--
-- This simply calls:
-- @
--     read s :: GeoPos
-- @
-- so 'error' should be handled at the call site.
--
readGeo :: String -> GeoPos
readGeo s = read s :: GeoPos

-- | Same as 'readGeo' but returns a 'MonadFail'.
readGeoF :: (MonadFail m) => String -> m GeoPos
readGeoF s =
    let pg = readEither s
     in case pg of
            Left e -> fail e
            Right g -> return g

-- | Same as 'readGeo' but returns a 'Maybe'.
readGeoM :: String -> Maybe GeoPos
readGeoM = readMaybe

-- | Is given latitude in range [-90, 90]?
isValidLatitude :: (Ord a, Num a) => a -> Bool
isValidLatitude lat = lat >= -90 && lat <= 90

-- | Is given longitude in range [-180, 180]?
isValidLongitude :: (Ord a, Num a) => a -> Bool
isValidLongitude lon = lon >= -180 && lon <= 180

-- | Parses and returns a 'GeoPos'.
geo' :: ReadP GeoPos
geo' = geoSymbol <|> geoNoSymbol

-- | Parses and returns a 'GeoPos', d°m's'' expected.
geoSymbol :: ReadP GeoPos
geoSymbol = do
    lat <- latSymbol
    skipSpaces
    lon <- lonSymbol
    return (geo lat lon)

-- | Parses and returns a 'GeoPos', DDMMSS expected.
geoNoSymbol :: ReadP GeoPos
geoNoSymbol = do
    lat <- latNoSymbol
    lon <- lonNoSymbol
    return (geo lat lon)

-- | Parses and returns a latitude, d°m's'' expected.
latSymbol :: ReadP Double
latSymbol = do
    d <- number'
    _ <- string degSymbol
    ms <- option (0, 0) (minSecSymbol <|> minuteSymbol)
    h <- hemisphere
    decLat d ms h

-- | Parses and returns a latitude, DDMMSS expected.
latNoSymbol :: ReadP Double
latNoSymbol = do
    d <- number 2
    ms <- option (0, 0) (minSecNoSymbol <|> minuteNoSymbol)
    h <- hemisphere
    decLat d ms h

-- | Parses N or S char.
hemisphere :: ReadP Char
hemisphere = char 'N' <|> char 'S'

-- | Parses and returns a longitude, d°m's'' expected.
lonSymbol :: ReadP Double
lonSymbol = do
    d <- number'
    _ <- string degSymbol
    ms <- option (0, 0) (minSecSymbol <|> minuteSymbol)
    m <- meridian
    decLon d ms m

-- | Parses and returns a longitude, DDDMMSS expected.
lonNoSymbol :: ReadP Double
lonNoSymbol = do
    d <- number 3
    ms <- option (0, 0) (minSecNoSymbol <|> minuteNoSymbol)
    m <- meridian
    decLon d ms m

-- | Parses E or W char.
meridian :: ReadP Char
meridian = char 'E' <|> char 'W'

-- | Parses minute and second, m's'' expected.
minSecSymbol :: ReadP (Int, Int)
minSecSymbol = do
    m <- number'
    _ <- string minSymbol
    s <- number'
    _ <- string secSymbol
    return (m, s)

-- | Parses minute and second, MMSS expected.
minSecNoSymbol :: ReadP (Int, Int)
minSecNoSymbol = do
    m <- number 2
    s <- number 2
    return (m, s)

-- | Parses minute, m' expected.
minuteSymbol :: ReadP (Int, Int)
minuteSymbol = do
    m <- number'
    _ <- string minSymbol
    return (m, 0)

-- | Parses minute, MM expected.
minuteNoSymbol :: ReadP (Int, Int)
minuteNoSymbol = do
    m <- number 2
    return (m, 0)

degSymbol :: String
degSymbol = "°"

minSymbol :: String
minSymbol = "'"

secSymbol :: String
secSymbol = "''"

-- | Parses the given number of digits and returns the read number.
number :: Int -> ReadP Int
number n = fmap read (count n digit)

-- | Parses 0 or digits and returns the read number.
number' :: ReadP Int
number' = fmap read (many digit)

-- | Parses a digit.
digit :: ReadP Char
digit = satisfy isDigit

-- | Converts latitude in degrees minutes and seconds to a decimal latitude.
-- fails if any component is invalid.
decLat :: Int -> (Int, Int) -> Char -> ReadP Double
decLat de (mi, se) h =
    if not (isValidLatitude de)
        then pfail
        else decimal de mi se (h == 'N')

-- | Converts longitude in degrees minutes and seconds to a decimal longitude.
-- fails if any component is invalid.
decLon :: Int -> (Int, Int) -> Char -> ReadP Double
decLon de (mi, se) m =
    if not (isValidLongitude de)
        then pfail
        else decimal de mi se (m == 'E')

-- | Converts degrees minutes and seconds to a decimal degrees.
-- fails if minute or second is invalid
decimal :: Int -> Int -> Int -> Bool -> ReadP Double
decimal d m s p
    | m < 0 || m > 59 || s < 0 || s > 59 = pfail
    | p = return v
    | otherwise = return (-v)
  where
    v = fromIntegral d + fromIntegral m / 60.0 + fromIntegral s / 3600.0

-- | Latitude to string.
showLat :: Double -> String
showLat lat
    | lat >= 0.0 = dms ++ "N"
    | otherwise = dms ++ "S"
  where
    dms = showDms (abs lat)

-- | Longitude to string.
showLon :: Double -> String
showLon lon
    | lon >= 0.0 = dms ++ "E"
    | otherwise = dms ++ "W"
  where
    dms = showDms (abs lon)

-- | Decima degrees (absolute value expected) to string.
showDms :: Double -> String
showDms dms = show di ++ degSymbol ++ show mi ++ minSymbol ++ show si ++ secSymbol
  where
    di = truncate dms :: Int
    ms = (dms - fromIntegral di) * 60.0
    mi = truncate ms :: Int
    si = truncate ((ms - fromIntegral mi) * 60.0) :: Int
