-- |
-- Module:      Data.Geo.Jord.LatLong
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Functions related to latitudes & longitudes.
--
module Data.Geo.Jord.LatLong
    ( isValidLatLong
    , latLongDmsP
    , latLongDmsCompactP
    , latLongDmsSymbolsP
    , showLatLong
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Fail (MonadFail)
import Data.Char ()
import Data.Maybe ()
import Text.ParserCombinators.ReadP (ReadP, char, option, pfail)

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Model
import Data.Geo.Jord.Parser

-- | @isValidLatLong lat lon m@ determines whether the given latitude & longitude are
-- both valid for model @m@.
isValidLatLong :: (Model a) => Angle -> Angle -> a -> Bool
isValidLatLong lat lon m = isValidLat lat && isValidLong lon m

-- | latitude and longitude reader.
-- Formats:
--
--     * DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - e.g. 553621N0130002E or 0116S03649E or 47N122W
--
--     * 'Angle'[N|S] 'Angle'[E|W] - e.g. 55°36'21''N 13°0'02''E or 11°16'S 36°49'E or 47°N 122°W
--
latLongDmsP :: (Model a) => a -> ReadP (Angle, Angle)
latLongDmsP m = latLongDmsCompactP m <|> latLongDmsSymbolsP m

-- | reads latitude and longitude in DD(D)MMSS.
latLongDmsCompactP :: (Model a) => a -> ReadP (Angle, Angle)
latLongDmsCompactP m = do
    lat <- blat
    lon <- blon
    if isValidLatLong lat lon m
        then return (lat, lon)
        else pfail

-- | reads latitude in DDMMSS.
blat :: ReadP Angle
blat = do
    d' <- digits 2
    (m', s') <- option (0, 0.0) (ms <|> mi)
    h <- hemisphere
    if h == 'N'
        then dmsF d' m' s'
        else dmsF (-d') m' s'

-- | reads longitude in DDDMMSS.
blon :: ReadP Angle
blon = do
    d' <- digits 3
    (m', s') <- option (0, 0.0) (ms <|> mi)
    m'' <- meridian
    if m'' == 'E'
        then dmsF d' m' s'
        else dmsF (-d') m' s'

-- | reads N or S char.
hemisphere :: ReadP Char
hemisphere = char 'N' <|> char 'S'

-- | reads E or W char.
meridian :: ReadP Char
meridian = char 'E' <|> char 'W'

-- | reads minutes and seconds.
ms :: ReadP (Int, Double)
ms = do
    m' <- digits 2
    s' <- digits 2
    return (m', fromIntegral s')

-- | reads minutes.
mi :: ReadP (Int, Double)
mi = do
    m' <- digits 2
    return (m', 0.0)

-- | reads (latitude, longitude) from a human friendly text - see 'Angle'.
latLongDmsSymbolsP :: (Model a) => a -> ReadP (Angle, Angle)
latLongDmsSymbolsP m = do
    lat <- hlat
    _ <- char ' ' <|> char ','
    lon <- hlon
    if isValidLatLong lat lon m
        then return (lat, lon)
        else pfail

-- | reads a latitude, 'Angle'N|S expected.
hlat :: ReadP Angle
hlat = do
    lat <- angleP
    h <- hemisphere
    if h == 'N'
        then return lat
        else return (negate' lat)

-- | reads a longitude, 'Angle'E|W expected.
hlon :: ReadP Angle
hlon = do
    lon <- angleP
    m' <- meridian
    if m' == 'E'
        then return lon
        else return (negate' lon)

-- | Show a (latitude, longitude) pair as DMS - e.g. 55°36'21''N,13°0'2''E.
showLatLong :: (Angle, Angle) -> String
showLatLong (lat, lon) = showLat lat ++ "," ++ showLon lon

-- | Latitude to string.
showLat :: Angle -> String
showLat lat
    | isNegative lat = show (negate' lat) ++ "S"
    | otherwise = show lat ++ "N"

-- | Longitude to string.
showLon :: Angle -> String
showLon lon
    | isNegative lon = show (negate' lon) ++ "W"
    | otherwise = show lon ++ "E"

dmsF :: (MonadFail m) => Int -> Int -> Double -> m Angle
dmsF degs mins secs =
    case e of
        Left err -> fail err
        Right a -> return a
  where
    e = dms degs mins secs

isValidLat :: Angle -> Bool
isValidLat a = isWithin a (decimalDegrees (-90)) (decimalDegrees 90)

isValidLong :: (Model a) => Angle -> a -> Bool
isValidLong a m =
    case longitudeRange m of
        L180 -> isWithin a (decimalDegrees (-180)) (decimalDegrees 180)
        L360 -> isWithin a (decimalDegrees 0) (decimalDegrees 360)
