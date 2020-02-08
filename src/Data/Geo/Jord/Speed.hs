-- |
-- Module:      Data.Geo.Jord.Speed
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with speed in metres per second, kilometres per hour, miles per hour, knots or feet per second.
--
module Data.Geo.Jord.Speed
    (
    -- * The 'Speed' type
      Speed
    -- * Smart constructors
    , averageSpeed
    , metresPerSecond
    , kilometresPerHour
    , milesPerHour
    , knots
    , feetPerSecond
    -- * Read
    , speedP
    , readSpeed
    -- * Conversions
    , toMetresPerSecond
    , toKilometresPerHour
    , toMilesPerHour
    , toKnots
    , toFeetPerSecond
    ) where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP (ReadP, pfail, readP_to_S, skipSpaces, string)
import Text.Read (readMaybe)

import Data.Geo.Jord.Duration
import Data.Geo.Jord.Length
import Data.Geo.Jord.Parser
import Data.Geo.Jord.Quantity

-- | A speed with a resolution of 1 millimetre per hour.
newtype Speed =
    Speed
        { mmPerHour :: Int
        }
    deriving (Eq)

-- | See 'speedP'.
instance Read Speed where
    readsPrec _ = readP_to_S speedP

-- | Speed is shown in kilometres per hour.
instance Show Speed where
    show s = show (toKilometresPerHour s) ++ "km/h"

instance Ord Speed where
    (<=) (Speed s1) (Speed s2) = s1 <= s2

-- | Add/Subtract Speed.
instance Quantity Speed where
    add a b = Speed (mmPerHour a + mmPerHour b)
    sub a b = Speed (mmPerHour a - mmPerHour b)
    zero = Speed 0

-- | 'Speed' from covered distance and duration.
averageSpeed :: Length -> Duration -> Speed
averageSpeed d t = Speed (round (mm / h))
  where
    mm = toMillimetres d
    h = toHours t

-- | 'Speed' from given amount of metres per second.
metresPerSecond :: Double -> Speed
metresPerSecond mps = Speed (round (mps * 3600000.0))

-- | 'Speed' from given amount of kilometres per hour.
kilometresPerHour :: Double -> Speed
kilometresPerHour kph = Speed (round (kph * 1e+6))

-- | 'Speed' from given amount of miles per hour.
milesPerHour :: Double -> Speed
milesPerHour mph = Speed (round (mph * 1609344.0))

-- | 'Speed' from given amount of knots.
knots :: Double -> Speed
knots kt = Speed (round (kt * 1852000.0))

-- | 'Speed' from given amount of feet per second.
feetPerSecond :: Double -> Speed
feetPerSecond fps = Speed (round (fps * 1097280.0))

-- | Reads an a 'Speed' from the given string using 'speedP'.
readSpeed :: String -> Maybe Speed
readSpeed s = readMaybe s :: (Maybe Speed)

-- | @toMetresPerSecond s@ converts @s@ to metres per second.
toMetresPerSecond :: Speed -> Double
toMetresPerSecond (Speed s) = fromIntegral s / 3600000.0

-- | @toKilometresPerHour s@ converts @s@ to kilometres per hour.
toKilometresPerHour :: Speed -> Double
toKilometresPerHour (Speed s) = fromIntegral s / 1e+6

-- | @toMilesPerHour s@ converts @s@ to miles per hour.
toMilesPerHour :: Speed -> Double
toMilesPerHour (Speed s) = fromIntegral s / 1609344.0

-- | @toKnots s@ converts @s@ to knots.
toKnots :: Speed -> Double
toKnots (Speed s) = fromIntegral s / 1852000.0

-- | @toFeetPerSecond s@ converts @s@ to feet per second.
toFeetPerSecond :: Speed -> Double
toFeetPerSecond (Speed s) = fromIntegral s / 1097280.0

-- | Parses and returns a 'Speed' formatted as (-)float[m/s|km/h|mph|kt].
-- e.g. 300m/s, 250km/h, -154mph, 400kt or 100ft/s.
speedP :: ReadP Speed
speedP = do
    s <- number
    skipSpaces
    u <- string "m/s" <|> string "km/h" <|> string "mph" <|> string "kt" <|> string "ft/s"
    case u of
        "m/s" -> return (metresPerSecond s)
        "km/h" -> return (kilometresPerHour s)
        "mph" -> return (milesPerHour s)
        "kt" -> return (knots s)
        "ft/s" -> return (feetPerSecond s)
        _ -> pfail
