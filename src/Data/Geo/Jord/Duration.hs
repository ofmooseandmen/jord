-- |
-- Module:      Data.Geo.Jord.Duration
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with (signed) durations.
--
-- In order to use this module you should start with the following imports:
--
-- @
-- import Data.Geo.Jord.Duration (Duration)
-- import qualified Data.Geo.Jord.Duration as Duration
-- @
module Data.Geo.Jord.Duration
    (
    -- * The 'Duration' type
      Duration
    , toMilliseconds
    -- * Smart constructors
    , milliseconds
    , hours
    , minutes
    , seconds
    , hms
    -- * Conversions
    , toHours
    , toMinutes
    , toSeconds
    -- * Read
    , duration
    , read
    -- * Misc
    , add
    , subtract
    , zero
    ) where

import Prelude hiding (read, subtract)
import Text.ParserCombinators.ReadP (ReadP, char, option, readP_to_S)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Data.Geo.Jord.Parser

-- | A duration with a resolution of 1 millisecond.
newtype Duration =
    Duration
        { toMilliseconds :: Int -- ^ the number of milliseconds in duration.
        }
    deriving (Eq)

-- | See 'duration'.
instance Read Duration where
    readsPrec _ = readP_to_S duration

-- | Show 'Duration' as @(-)nHnMn.nS@.
instance Show Duration where
    show d@(Duration millis) =
        show h ++ "H" ++ show m ++ "M" ++ show s ++ "." ++ printf "%03d" ms ++ "S"
      where
        h = truncate (toHours d) :: Int
        m = truncate (fromIntegral (millis `mod` 3600000) / 60000.0 :: Double) :: Int
        s = truncate (fromIntegral (millis `mod` 60000) / 1000.0 :: Double) :: Int
        ms = mod (abs millis) 1000

instance Ord Duration where
    (<=) (Duration d1) (Duration d2) = d1 <= d2

-- | Adds 2 durations.
add :: Duration -> Duration -> Duration
add a b = Duration (toMilliseconds a + toMilliseconds b)

-- | Subtracts 2 durations.
subtract :: Duration -> Duration -> Duration
subtract a b = Duration (toMilliseconds a - toMilliseconds b)

-- | 0 duration.
zero :: Duration
zero = Duration 0

-- | 'Duration' from hours minutes and decimal seconds.
hms :: Int -> Int -> Double -> Duration
hms h m s = milliseconds (fromIntegral h * 3600000 + fromIntegral m * 60000 + s * 1000)

-- | 'Duration' from given amount of hours.
hours :: Double -> Duration
hours h = milliseconds (h * 3600000)

-- | 'Duration' from given amount of minutes.
minutes :: Double -> Duration
minutes m = milliseconds (m * 60000)

-- | 'Duration' from given amount of seconds.
seconds :: Double -> Duration
seconds s = milliseconds (s * 1000)

-- | 'Duration' from given amount of milliseconds.
milliseconds :: Double -> Duration
milliseconds ms = Duration (round ms)

-- | @toHours d@ gets the number of hours in @d@.
toHours :: Duration -> Double
toHours (Duration ms) = fromIntegral ms / 3600000.0 :: Double

-- | @toMinutes d@ gets the number of minutes in @d@.
toMinutes :: Duration -> Double
toMinutes (Duration ms) = fromIntegral ms / 60000.0 :: Double

-- | @toSeconds d@ gets the number of seconds in @d@.
toSeconds :: Duration -> Double
toSeconds (Duration ms) = fromIntegral ms / 1000.0 :: Double

-- | Reads a 'Duration' from the given string using 'duration'.
read :: String -> Maybe Duration
read s = readMaybe s :: (Maybe Duration)

-- | Parses and returns an 'Duration' formatted @(-)nHnMn.nS@.
duration :: ReadP Duration
duration = do
    h <- option 0 hoursP
    m <- option 0 minutesP
    s <- option 0.0 secondsP
    return (milliseconds (h * 3600000.0 + m * 60000.0 + s * 1000.0))

hoursP :: ReadP Double
hoursP = do
    h <- integer
    _ <- char 'H'
    return (fromIntegral h :: Double)

minutesP :: ReadP Double
minutesP = do
    m <- integer
    _ <- char 'M'
    return (fromIntegral m :: Double)

secondsP :: ReadP Double
secondsP = do
    s <- number
    _ <- char 'S'
    return s
