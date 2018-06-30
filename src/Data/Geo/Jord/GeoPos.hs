-- |
-- TODO add doc
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

newtype Degrees = Degrees
    { degrees :: Double
    } deriving (Eq, Show)

data GeoPos = GeoPos
    { latitude :: Degrees
    , longitude :: Degrees
    } deriving (Eq)

instance Read GeoPos where
    readsPrec _ = readP_to_S geo'

instance Show GeoPos where
    show (GeoPos lat lon) = showLat (degrees lat) ++ " " ++ showLon (degrees lon)

geo :: Double -> Double -> GeoPos
geo lat lon
    | not (isValidLatitude lat) = error ("invalid latitude=" ++ show lat)
    | not (isValidLongitude lon) = error ("invalid longitude=" ++ show lon)
    | otherwise = GeoPos (Degrees lat) (Degrees lon)

readGeo :: String -> GeoPos
readGeo s = read s :: GeoPos

readGeoF :: (MonadFail m) => String -> m GeoPos
readGeoF s =
    let pg = readEither s
     in case pg of
            Left e -> fail e
            Right g -> return g

readGeoM :: String -> Maybe GeoPos
readGeoM = readMaybe

isValidLatitude :: (Ord a, Num a) => a -> Bool
isValidLatitude lat = lat >= -90 && lat <= 90

isValidLongitude :: (Ord a, Num a) => a -> Bool
isValidLongitude lon = lon >= -180 && lon <= 180

geo' :: ReadP GeoPos
geo' = geoSep <|> geoNoSep

geoSep :: ReadP GeoPos
geoSep = do
    lat <- latSep
    skipSpaces
    lon <- lonSep
    return (geo lat lon)

geoNoSep :: ReadP GeoPos
geoNoSep = do
    lat <- latNoSep
    lon <- lonNoSep
    return (geo lat lon)

latSep :: ReadP Double
latSep = do
    d <- number'
    _ <- string degSymbol
    ms <- option (0, 0) (minSecSep <|> minuteSep)
    h <- hemisphere
    decLat d ms h

latNoSep :: ReadP Double
latNoSep = do
    d <- number 2
    ms <- option (0, 0) (minSecNoSep <|> minuteNoSep)
    h <- hemisphere
    decLat d ms h

hemisphere :: ReadP Char
hemisphere = char 'N' <|> char 'S'

lonSep :: ReadP Double
lonSep = do
    d <- number'
    _ <- string degSymbol
    ms <- option (0, 0) (minSecSep <|> minuteSep)
    m <- meridian
    decLon d ms m

lonNoSep :: ReadP Double
lonNoSep = do
    d <- number 3
    ms <- option (0, 0) (minSecNoSep <|> minuteNoSep)
    m <- meridian
    decLon d ms m

meridian :: ReadP Char
meridian = char 'E' <|> char 'W'

minSecSep :: ReadP (Int, Int)
minSecSep = do
    m <- number'
    _ <- string minSymbol
    s <- number'
    _ <- string secSymbol
    return (m, s)

minSecNoSep :: ReadP (Int, Int)
minSecNoSep = do
    m <- number 2
    s <- number 2
    return (m, s)

minuteSep :: ReadP (Int, Int)
minuteSep = do
    m <- number'
    _ <- string minSymbol
    return (m, 0)

minuteNoSep :: ReadP (Int, Int)
minuteNoSep = do
    m <- number 2
    return (m, 0)

degSymbol :: String
degSymbol = "°"

minSymbol :: String
minSymbol = "'"

secSymbol :: String
secSymbol = "''"

number :: Int -> ReadP Int
number n = fmap read (count n digit)

number' :: ReadP Int
number' = fmap read (many digit)

digit :: ReadP Char
digit = satisfy isDigit

decLat :: Int -> (Int, Int) -> Char -> ReadP Double
decLat de (mi, se) h =
    if not (isValidLatitude de)
        then pfail
        else decimal de mi se (h == 'N')

decLon :: Int -> (Int, Int) -> Char -> ReadP Double
decLon de (mi, se) m =
    if not (isValidLongitude de)
        then pfail
        else decimal de mi se (m == 'E')

decimal :: Int -> Int -> Int -> Bool -> ReadP Double
decimal d m s p
    | m < 0 || m > 59 || s < 0 || s > 59 = pfail
    | p = return v
    | otherwise = return (-v)
  where
    v = fromIntegral d + fromIntegral m / 60.0 + fromIntegral s / 3600.0

showLat :: Double -> String
showLat lat
    | lat >= 0.0 = dms ++ "N"
    | otherwise = dms ++ "S"
  where
    dms = showDms (abs lat)

showLon :: Double -> String
showLon lon
    | lon >= 0.0 = dms ++ "E"
    | otherwise = dms ++ "W"
  where
    dms = showDms (abs lon)

showDms :: Double -> String
showDms dms = show di ++ degSymbol ++ show mi ++ minSymbol ++ show si ++ secSymbol
  where
    di = truncate dms :: Int
    ms = (dms - fromIntegral di) * 60.0
    mi = truncate ms :: Int
    si = truncate ((ms - fromIntegral mi) * 60.0) :: Int
