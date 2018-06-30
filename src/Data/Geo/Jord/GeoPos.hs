-- |
-- TODO add doc
--
module Data.Geo.Jord.GeoPos
    ( Degrees(..)
    , GeoPos(latitude, longitude)
    , geo
    , readGeo
    , readGeoE
    , readGeoM
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Char
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP
import Text.Read hiding (choice, pfail)

newtype Degrees = Degrees
    { degrees :: Double
    } deriving (Eq, Show)

data GeoPos = GeoPos
    { latitude :: Degrees
    , longitude :: Degrees
    } deriving (Eq, Show)

instance Read GeoPos where
    readsPrec _ = readP_to_S geo'

geo :: Double -> Double -> GeoPos
geo lat lon
    | not (isValidLatitude lat) = error ("invalid latitude=" ++ show lat)
    | not (isValidLongitude lon) = error ("invalid longitude=" ++ show lon)
    | otherwise = GeoPos (Degrees lat) (Degrees lon)

readGeo :: (MonadFail m) => String -> m GeoPos
readGeo s =
    let pg = readGeoE s
     in case pg of
            Left e -> fail e
            Right g -> return g

readGeoE :: String -> Either String GeoPos
readGeoE s = readEither s

readGeoM :: String -> Maybe GeoPos
readGeoM s = readMaybe s

isValidLatitude :: (Ord a, Num a) => a -> Bool
isValidLatitude lat = lat >= -90 && lat <= 90

isValidLongitude :: (Ord a, Num a) => a -> Bool
isValidLongitude lon = lon >= -180 && lon <= 180

geo' :: ReadP GeoPos
geo' = do
    lat <- latitude'
    skipSpaces
    lon <- longitude'
    return (geo lat lon)

latitude' :: ReadP Double
latitude' = do
    d <- deg 2
    ms <- option 0 mOrMs
    h <- char 'N' <|> char 'S'
    return (decimal d ms (h == 'N'))

longitude' :: ReadP Double
longitude' = do
    d <- deg 3
    ms <- option 0 mOrMs
    h <- char 'E' <|> char 'W'
    return (decimal d ms (h == 'E'))

deg :: Int -> ReadP Double
deg n = do
    d <- numbers n
    _ <- option ' ' (char '°')
    if n == 2 && not (isValidLatitude d)
        then pfail
        else if n == 3 && not (isValidLongitude d)
                 then pfail
                 else return (fromIntegral d)

mOrMs :: ReadP Double
mOrMs = choice [minSec, minute]

minSec :: ReadP Double
minSec = do
    m <- numbers 2
    _ <- option ' ' (char '\'')
    s <- numbers 2
    _ <- option "" (string "''")
    if not (isValidMinute m)
        then pfail
        else if not (isValidSecond s)
                 then pfail
                 else return (fromIntegral m / 60.0 + fromIntegral s / 3600.0)

minute :: ReadP Double
minute = do
    m <- numbers 2
    _ <- option ' ' (char '\'')
    if not (isValidMinute m)
        then pfail
        else return (fromIntegral m / 60.0)

digit :: ReadP Char
digit = satisfy isDigit

numbers :: Int -> ReadP Int
numbers n = fmap read (count n digit)

isValidMinute :: Int -> Bool
isValidMinute m = m >= 0 && m <= 59

isValidSecond :: Int -> Bool
isValidSecond s = s >= 0 && s <= 59

decimal :: Double -> Double -> Bool -> Double
decimal d ms positive
    | positive = d + ms
    | otherwise = -(d + ms)
