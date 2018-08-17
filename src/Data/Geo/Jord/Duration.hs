-- |
-- Module:      Data.Geo.Jord.Duration
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with (signed) durations.
--
module Data.Geo.Jord.Duration
    (
    -- * The 'Duration' type
      Duration(toMilliseconds)
    -- * Smart constructors
    , milliseconds
    , hours
    , minutes
    , seconds
   -- * Accessors
    , toHours
    , toMinutes
    , toSeconds
   -- * Read
    , readDuration
    , readDurationE
    , readDurationF
    ) where

import Control.Monad.Fail
import Data.Geo.Jord.Parse
import Data.Geo.Jord.Quantity
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP
import Text.Printf
import Text.Read hiding (pfail)

-- | A durartion with a resolution of 1 millisecond.
newtype Duration = Duration
    { toMilliseconds :: Int
    } deriving (Eq)

-- | See 'readDuration'.
instance Read Duration where
    readsPrec _ = readP_to_S duration

-- | show Duration as @(-)nHnMn.nS@.
instance Show Duration where
    show d@(Duration millis) =
        show h ++ "H" ++ show m ++ "M" ++ show s ++ "." ++ printf "%03d" ms ++ "S"
      where
        h = toHours d
        m = truncate (fromIntegral (millis `mod` 3600000) / 60000.0 :: Double) :: Int
        s = truncate (fromIntegral (millis `mod` 60000) / 1000.0 :: Double) :: Int
        ms = mod (abs millis) 1000

-- | Add/Subtract Durations.
instance Quantity Duration where
    add a b = Duration (toMilliseconds a + toMilliseconds b)
    sub a b = Duration (toMilliseconds a - toMilliseconds b)
    zero = Duration 0

-- | 'Duration' from given amount of milliseconds.
milliseconds :: Int -> Duration
milliseconds = Duration

-- | 'Duration' from given amount of seconds.
seconds :: Int -> Duration
seconds s = Duration (s * 1000)

-- | 'Duration' from given amount of minutes.
minutes :: Int -> Duration
minutes m = Duration (m * 60000)

-- | 'Duration' from given amount of hours.
hours :: Int -> Duration
hours h = Duration (h * 3600000)

-- | @toHours d@ gets the number of hours in @d@.
toHours :: Duration -> Int
toHours (Duration ms) = truncate (fromIntegral ms / 3600000.0 :: Double)

-- | @toMinutes d@ gets the number of minutes in @d@.
toMinutes :: Duration -> Int
toMinutes (Duration ms) = truncate (fromIntegral ms / 60000.0 :: Double)

-- | @toSeconds d@ gets the number of seconds in @d@.
toSeconds :: Duration -> Int
toSeconds (Duration ms) = truncate (fromIntegral ms / 1000.0 :: Double)

-- | Obtains a 'Duration' from the given string formatted @(-)nHnMn.nS@.
--
-- This simply calls @read s :: Duration@ so 'error' should be handled at the call site.
--
readDuration :: String -> Duration
readDuration s = read s :: Duration

-- | Same as 'readDuration' but returns a 'Either'.
readDurationE :: String -> Either String Duration
readDurationE s =
    case readMaybe s of
        Nothing -> Left ("couldn't read duration " ++ s)
        Just l -> Right l

-- | Same as 'readDuration' but returns a 'MonadFail'.
readDurationF :: (MonadFail m) => String -> m Duration
readDurationF s =
    let p = readEither s
     in case p of
            Left e -> fail e
            Right l -> return l

-- | Parses and returns an 'Duration'.
duration :: ReadP Duration
duration = do
    h <- option 0 hoursP
    m <- option 0 minutesP
    s <- option 0.0 secondsP
    return (Duration (h * 3600000 + m * 60000 + (truncate s * 1000)))

hoursP :: ReadP Int
hoursP = do
    h <- integer
    _ <- char 'H'
    return h

minutesP :: ReadP Int
minutesP = do
    m <- integer
    _ <- char 'M'
    return m

secondsP :: ReadP Double
secondsP = do
    s <- double
    _ <- char 'S'
    return s
