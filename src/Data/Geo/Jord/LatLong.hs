-- |
-- Module:      Data.Geo.Jord.LatLong
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Geodetic latitude and longitude.
--
module Data.Geo.Jord.LatLong
    (
    -- * The 'LatLong' type
      LatLong
    , latitude
    , longitude
    -- * Smart constructors
    , latLong
    , latLongE
    , latLongF
    , decimalLatLong
    , decimalLatLongE
    , decimalLatLongF
    -- * read
    , readLatLong
    , readLatLongE
    , readLatLongF
    -- * Misc.
    , toDecimalDegrees'
    ) where

import Control.Applicative hiding (many)
import Control.Monad.Fail
import Data.Char
import Data.Geo.Jord.Angle
import Data.Geo.Jord.Parse
import Data.Maybe
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)

-- | Horizontal position defined by its geodetic latitude and longitude.
data LatLong = LatLong
    { latitude :: Angle -- ^ geodetic latitude
    , longitude :: Angle -- ^ longitude
    } deriving (Eq)

-- | See 'readLatLong'.
instance Read LatLong where
    readsPrec _ = readP_to_S ll

-- | Produced string format: d°(m')(s'')[N|S],d°(m')(s'')[E|W] - e.g. 55°36'21''N,13°0'2''E.
instance Show LatLong where
    show (LatLong lat lon) = showLat lat ++ "," ++ showLon lon

-- | 'LatLong' from given latitude and longitude.
-- 'error's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
latLong :: Angle -> Angle -> LatLong
latLong lat lon =
    fromMaybe
        (error ("Invalid latitude=" ++ show lat ++ " or longitude=" ++ show lon))
        (latLongF lat lon)

-- | 'LatLong' from given latitude and longitude.
-- A 'Left' indicates that the given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
latLongE :: Angle -> Angle -> Either String LatLong
latLongE lat lon
    | not (isWithin lat (decimalDegrees (-90)) (decimalDegrees 90)) =
        Left ("Invalid latitude=" ++ show lat)
    | not (isWithin lon (decimalDegrees (-180)) (decimalDegrees 180)) =
        Left ("Invalid longitude=" ++ show lon)
    | otherwise = Right (LatLong lat lon)

-- | 'LatLong' from given latitude and longitude.
-- 'fail's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
latLongF :: (MonadFail m) => Angle -> Angle -> m LatLong
latLongF lat lon =
    case e of
        Left err -> fail err
        Right g -> return g
  where
    e = latLongE lat lon

-- | 'LatLong' from given latitude and longitude in __decimal degrees__.
-- 'error's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
decimalLatLong :: Double -> Double -> LatLong
decimalLatLong lat lon = latLong (decimalDegrees lat) (decimalDegrees lon)

-- | 'LatLong' from given latitude and longitude in __decimal degrees__.
-- A 'Left' indicates that the given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
decimalLatLongE :: Double -> Double -> Either String LatLong
decimalLatLongE lat lon = latLongE (decimalDegrees lat) (decimalDegrees lon)

-- | 'LatLong' from given latitude and longitude in __decimal degrees__.
-- 'fail's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
decimalLatLongF :: (MonadFail m) => Double -> Double -> m LatLong
decimalLatLongF lat lon = latLongF (decimalDegrees lat) (decimalDegrees lon)

-- | Obtains a 'LatLong' from the given string formatted as either:
--
--     * DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - e.g. 553621N0130002E or 0116S03649E or 47N122W
--
--     * 'Angle'[N|S] 'Angle'[E|W] - e.g. 55°36'21''N 13°0'02''E or 11°16'S 36°49'E or 47°N 122°W
--
-- This simply calls @read s :: GeoPos@ so 'error' should be handled at the call site.
--
readLatLong :: String -> LatLong
readLatLong s = read s :: LatLong

-- | Same as 'readLatLong' but returns a 'Either'.
readLatLongE :: String -> Either String LatLong
readLatLongE s =
    case readMaybe s of
        Nothing -> Left ("couldn't read geo pos " ++ s)
        Just g -> Right g

-- | Same as 'readLatLong' but returns a 'MonadFail'.
readLatLongF :: (MonadFail m) => String -> m LatLong
readLatLongF s =
    let pg = readLatLongE s
     in case pg of
            Left e -> fail e
            Right g -> return g

-- | Converts the given 'LatLong' to tuple of latitude and longitude in decimal degrees.
toDecimalDegrees' :: LatLong -> (Double, Double)
toDecimalDegrees' g = (toDecimalDegrees (latitude g), toDecimalDegrees (longitude g))

-- | Parses and returns a 'LatLong'.
ll :: ReadP LatLong
ll = block <|> human

-- | Parses and returns a 'LatLong' - DD(D)MMSS.
block :: ReadP LatLong
block = do
    lat <- blat
    lon <- blon
    latLongF lat lon

-- | Parses and returns a latitude, DDMMSS expected.
blat :: ReadP Angle
blat = do
    d' <- digits 2
    (m', s') <- option (0, 0) (ms <|> m)
    h <- hemisphere
    if h == 'N'
        then dmsF d' m' s' 0
        else dmsF (-d') m' s' 0

-- | Parses and returns a longitude, DDDMMSS expected.
blon :: ReadP Angle
blon = do
    d' <- digits 3
    (m', s') <- option (0, 0) (ms <|> m)
    m'' <- meridian
    if m'' == 'E'
        then dmsF d' m' s' 0
        else dmsF (-d') m' s' 0

-- | Parses N or S char.
hemisphere :: ReadP Char
hemisphere = char 'N' <|> char 'S'

-- | Parses E or W char.
meridian :: ReadP Char
meridian = char 'E' <|> char 'W'

-- | Parses minutes and seconds.
ms :: ReadP (Int, Int)
ms = do
    m' <- digits 2
    s' <- digits 2
    return (m', s')

-- | Parses minutes.
m :: ReadP (Int, Int)
m = do
    m' <- digits 2
    return (m', 0)

-- | Parses and returns a 'LatLong' from a human friendly text - see 'Angle'.
human :: ReadP LatLong
human = do
    lat <- hlat
    _ <- char ' ' <|> char ','
    lon <- hlon
    latLongF lat lon

-- | Parses and returns a latitude, 'Angle'N|S expected.
hlat :: ReadP Angle
hlat = do
    lat <- angle
    h <- hemisphere
    if h == 'N'
        then return lat
        else return (negate' lat)

-- | Parses and returns a longitude, 'Angle'E|W expected.
hlon :: ReadP Angle
hlon = do
    lon <- angle
    m' <- meridian
    if m' == 'E'
        then return lon
        else return (negate' lon)

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
