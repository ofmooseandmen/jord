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
-- In order to use this module you should start with the following imports:
--
-- @
-- import Data.Geo.Jord.Speed (Speed)
-- import qualified Data.Geo.Jord.Speed as Speed
-- @
--
module Data.Geo.Jord.Speed
    (
    -- * The 'Speed' type
      Speed
    -- * Smart constructors
    , average
    , metresPerSecond
    , kilometresPerHour
    , milesPerHour
    , knots
    , feetPerSecond
    -- * Read
    , speed
    , read
    -- * Conversions
    , toMetresPerSecond
    , toKilometresPerHour
    , toMilesPerHour
    , toKnots
    , toFeetPerSecond
    -- * Misc
    , add
    , subtract
    , zero
    ) where

import Control.Applicative ((<|>))
import Prelude hiding (read, subtract)
import Text.ParserCombinators.ReadP (ReadP, pfail, readP_to_S, skipSpaces, string)
import Text.Read (readMaybe)

import Data.Geo.Jord.Duration (Duration)
import qualified Data.Geo.Jord.Duration as Duration (toHours)
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (toMillimetres)
import Data.Geo.Jord.Parser

-- | A speed with a resolution of 1 millimetre per hour.
newtype Speed =
    Speed
        { mmPerHour :: Int
        }
    deriving (Eq)

-- | See 'speed'.
instance Read Speed where
    readsPrec _ = readP_to_S speed

-- | Speed is shown in kilometres per hour.
instance Show Speed where
    show s = show (toKilometresPerHour s) ++ "km/h"

instance Ord Speed where
    (<=) (Speed s1) (Speed s2) = s1 <= s2

-- | Adds 2 speeds.
add :: Speed -> Speed -> Speed
add a b = Speed (mmPerHour a + mmPerHour b)

-- | Subtracts 2 speeds.
subtract :: Speed -> Speed -> Speed
subtract a b = Speed (mmPerHour a - mmPerHour b)

-- | 0 speed.
zero :: Speed
zero = Speed 0

-- | 'Speed' from covered distance and duration.
average :: Length -> Duration -> Speed
average d t = Speed (round (mm / h))
  where
    mm = Length.toMillimetres d
    h = Duration.toHours t

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

-- | Reads a 'Speed' from the given string using 'speed'.
read :: String -> Maybe Speed
read s = readMaybe s :: (Maybe Speed)

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
speed :: ReadP Speed
speed = do
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
