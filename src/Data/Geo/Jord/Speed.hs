-- |
-- Module:      Data.Geo.Jord.Speed
-- Copyright:   (c) 2018 Cedric Liegeois
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
    , metresPerSecond
    , kilometresPerHour
    , milesPerHour
    , knots
    , feetPerSecond
    -- * Read
    , readSpeed
    , readSpeedE
    , readSpeedF
    -- * Conversions
    , toMetresPerSecond
    , toKilometresPerHour
    , toMilesPerHour
    , toKnots
    , toFeetPerSecond
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Geo.Jord.Parser
import Data.Geo.Jord.Quantity
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)

-- | A speed with a resolution of 1 millimetre per hour.
newtype Speed = Speed
    { mmPerHour :: Int
    } deriving (Eq)

-- | See 'readSpeed'.
instance Read Speed where
    readsPrec _ = readP_to_S speed

-- | Speed is shown in kilometres per hour.
instance Show Speed where
    show s = show (toKilometresPerHour s) ++ "km/h"

-- | Add/Subtract Speed.
instance Quantity Speed where
    add a b = Speed (mmPerHour a + mmPerHour b)
    sub a b = Speed (mmPerHour a - mmPerHour b)
    zero = Speed 0

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

-- | Obtains a 'Speed' from the given string formatted as (-)float[m/s|km/h|mph|kt] - e.g. 300m/s, 250km/h, -154mph, 400kt or 100ft/s.
--
-- This simply calls @read s :: Speed@ so 'error' should be handled at the call site.
--
readSpeed :: String -> Speed
readSpeed s = read s :: Speed

-- | Same as 'readSpeed' but returns a 'Either'.
readSpeedE :: String -> Either String Speed
readSpeedE s =
    case readMaybe s of
        Nothing -> Left ("couldn't read speed " ++ s)
        Just l -> Right l

-- | Same as 'readSpeed' but returns a 'MonadFail'.
readSpeedF :: (MonadFail m) => String -> m Speed
readSpeedF s =
    let p = readEither s
     in case p of
            Left e -> fail e
            Right l -> return l

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

-- | Parses and returns a 'Speed'.
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
