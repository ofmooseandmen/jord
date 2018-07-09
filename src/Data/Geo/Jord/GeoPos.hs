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
    ( GeoPos(latitude, longitude)
    , geoPos
    , geoPosF
    , readGeoPos
    , readGeoPosE
    , readGeoPosF
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

-- | A geographic position (latitude and longitude).
data GeoPos = GeoPos
    { latitude :: Angle
    , longitude :: Angle
    } deriving (Eq)

-- | See 'readGeoPos'.
instance Read GeoPos where
    readsPrec _ = readP_to_S geo

-- | Produced string format: d°(m')(s'')[N|S],d°(m')(s'')[E|W] - e.g. 55°36'21''N,13°0'2''E.
instance Show GeoPos where
    show (GeoPos lat lon) = showLat lat ++ "," ++ showLon lon

-- | 'GeoPos' from given latitude and longitude.
-- 'error's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
geoPos :: Angle -> Angle -> GeoPos
geoPos lat lon =
    fromMaybe
        (error ("Invalid latitude=" ++ show lat ++ " or longitude=" ++ show lon))
        (geoPosF lat lon)

-- | 'GeoPos' from given latitude and longitude.
-- A 'Left' indicates that the given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
geoPosE :: Angle -> Angle -> Either String GeoPos
geoPosE lat lon
    | not (isWithin lat (-90) 90) = Left ("Invalid latitude=" ++ show lat)
    | not (isWithin lon (-180) 180) = Left ("Invalid longitude=" ++ show lon)
    | otherwise = Right (GeoPos lat lon)

-- | 'GeoPos' from given latitude and longitude.
-- 'fail's if given latitude is outisde [-90..90]° and/or
-- given longitude is outisde [-180..180]°.
geoPosF :: (MonadFail m) => Angle -> Angle -> m GeoPos
geoPosF lat lon =
    case e of
        Left err -> fail err
        Right g -> return g
  where
    e = geoPosE lat lon

-- | Obtains a 'GeoPos' from the given string formatted as either:
--
--     * DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - e.g. 553621N0130002E or 0116S03649E or 47N122W
--
--     * 'Angle'[N|S] 'Angle'[E|W] - e.g. 55°36'21''N 13°0'02''E or 11°16'S 36°49'E or 47°N 122°W
--
-- This simply calls @read s :: GeoPos@ so 'error' should be handled at the call site.
--
readGeoPos :: String -> GeoPos
readGeoPos s = read s :: GeoPos

-- | Same as 'readGeoPos' but returns a 'Either'.
readGeoPosE :: String -> Either String GeoPos
readGeoPosE s =
    case readMaybe s of
        Nothing -> Left ("couldn't read geo pos " ++ s)
        Just g -> Right g

-- | Same as 'readGeoPos' but returns a 'MonadFail'.
readGeoPosF :: (MonadFail m) => String -> m GeoPos
readGeoPosF s =
    let pg = readGeoPosE s
     in case pg of
            Left e -> fail e
            Right g -> return g

-- | Parses and returns a 'GeoPos'.
geo :: ReadP GeoPos
geo = block <|> human

-- | Parses and returns a 'GeoPos' - DD(D)MMSS.
block :: ReadP GeoPos
block = do
    lat <- blat
    lon <- blon
    geoPosF lat lon

-- | Parses and returns a latitude, DDMMSS expected.
blat :: ReadP Angle
blat = do
    d' <- digits 2
    (m', s') <- option (0, 0) (ms <|> m)
    h <- hemisphere
    if h == 'N'
        then angle d' m' s' 0
        else angle (-d') m' s' 0

-- | Parses and returns a longitude, DDDMMSS expected.
blon :: ReadP Angle
blon = do
    d' <- digits 3
    (m', s') <- option (0, 0) (ms <|> m)
    m'' <- meridian
    if m'' == 'E'
        then angle d' m' s' 0
        else angle (-d') m' s' 0

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

-- | Parses and returns a 'GeoPos' from a human friendly text - see 'Angle'.
human :: ReadP GeoPos
human = do
    lat <- hlat
    _ <- char ' ' <|> char ','
    lon <- hlon
    geoPosF lat lon

-- | Parses and returns a latitude, 'Angle'N|S expected.
hlat :: ReadP Angle
hlat = do
    lat <- angleParser
    h <- hemisphere
    if h == 'N'
        then return lat
        else return (neg lat)

-- | Parses and returns a longitude, 'Angle'E|W expected.
hlon :: ReadP Angle
hlon = do
    lon <- angleParser
    m' <- meridian
    if m' == 'E'
        then return lon
        else return (neg lon)

-- | Latitude to string.
showLat :: Angle -> String
showLat lat
    | isNegative lat = show (neg lat) ++ "S"
    | otherwise = show lat ++ "N"

-- | Longitude to string.
showLon :: Angle -> String
showLon lon
    | isNegative lon = show (neg lon) ++ "W"
    | otherwise = show lon ++ "E"
